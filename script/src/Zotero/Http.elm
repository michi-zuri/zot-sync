module Zotero.Http exposing
    ( Response, Error(..), errorToString
    , get, header, lastModifiedVersion
    , maxAttempts
    )

{-| GET requests to the Zotero API with the server's flow-control rules:

  - `Backoff: N` on any response: wait N seconds before the next request.
  - `429`, or `503` with `Retry-After: N`: wait (N seconds, or longer each
    time), then retry, up to `maxAttempts`.
  - network errors and timeouts: retry with growing delays.

Every other status is returned as a `Response`; the caller decides what 304,
404 or 409 mean. Requests use `BackendTask.Http.request`, which elm-pages does
not cache; `BackendTask.Http.get` would serve cached responses, which is wrong
for a sync tool.

@docs Response, Error, errorToString
@docs get, header, lastModifiedVersion
@docs maxAttempts

-}

import BackendTask exposing (BackendTask)
import BackendTask.Http
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script


{-| `body` is the decoded JSON for 2xx responses and the raw text (as a JSON
string) otherwise. Header names are lower-cased.
-}
type alias Response =
    { url : String
    , status : Int
    , headers : Dict String String
    , body : Decode.Value
    }


{-| -}
type Error
    = Unreachable { url : String, problem : String }
    | NotJson { url : String, body : String }


{-| -}
errorToString : Error -> String
errorToString error =
    case error of
        Unreachable { url, problem } ->
            "could not reach " ++ url ++ ": " ++ problem

        NotJson { url, body } ->
            "expected JSON from " ++ url ++ ", got: " ++ String.left 200 body


{-| -}
maxAttempts : Int
maxAttempts =
    5


type SendError
    = Transport { retryable : Bool, problem : String }
    | BadBody String


{-| -}
get : Bool -> String -> List ( String, String ) -> BackendTask Error Response
get verbose url requestHeaders =
    attempt verbose url requestHeaders 1


attempt : Bool -> String -> List ( String, String ) -> Int -> BackendTask Error Response
attempt verbose url requestHeaders n =
    -- elm-pages identifies a request by method, URL, headers and body. Naming
    -- the attempt keeps every retry a distinct request; Test.BackendTask, which
    -- replays responses by that identity, would otherwise answer a retry with
    -- the response that caused it. The API ignores the header.
    send url
        (if n > 1 then
            requestHeaders ++ [ ( "Zotero-Sync-Attempt", String.fromInt n ) ]

         else
            requestHeaders
        )
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\result ->
                case result of
                    Err (Transport { retryable, problem }) ->
                        if retryable && n < maxAttempts then
                            note verbose ("GET " ++ url ++ " failed (" ++ problem ++ "), retrying")
                                |> BackendTask.andThen (\_ -> Script.sleep (1000 * 2 ^ (n - 1)))
                                |> BackendTask.andThen (\_ -> attempt verbose url requestHeaders (n + 1))

                        else
                            BackendTask.fail (Unreachable { url = url, problem = problem })

                    Err (BadBody body) ->
                        BackendTask.fail (NotJson { url = url, body = body })

                    Ok response ->
                        let
                            retryAfter : Maybe Int
                            retryAfter =
                                header "retry-after" response |> Maybe.andThen String.toInt
                        in
                        if (response.status == 429 || (response.status == 503 && retryAfter /= Nothing)) && n < maxAttempts then
                            let
                                seconds : Int
                                seconds =
                                    Maybe.withDefault (5 * 2 ^ (n - 1)) retryAfter
                            in
                            note True ("Zotero asked to slow down (HTTP " ++ String.fromInt response.status ++ "); waiting " ++ String.fromInt seconds ++ "s")
                                |> BackendTask.andThen (\_ -> Script.sleep (seconds * 1000))
                                |> BackendTask.andThen (\_ -> attempt verbose url requestHeaders (n + 1))

                        else
                            note verbose ("GET " ++ url ++ " -> " ++ String.fromInt response.status)
                                |> BackendTask.andThen (\_ -> honourBackoff response)
            )


honourBackoff : Response -> BackendTask error Response
honourBackoff response =
    case header "backoff" response |> Maybe.andThen String.toInt of
        Just seconds ->
            Script.log ("Zotero asked clients to back off; pausing " ++ String.fromInt seconds ++ "s")
                |> BackendTask.andThen (\_ -> Script.sleep (seconds * 1000))
                |> BackendTask.map (\_ -> response)

        Nothing ->
            BackendTask.succeed response


send : String -> List ( String, String ) -> BackendTask SendError Response
send url requestHeaders =
    BackendTask.Http.request
        { url = url
        , method = "GET"
        , headers = requestHeaders
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Just 120000
        }
        (BackendTask.Http.expectJson Decode.value |> BackendTask.Http.withMetadata (toResponse url))
        -- elm-pages would print a timing line per request; --verbose logs our own
        |> BackendTask.quiet
        |> BackendTask.onError
            (\{ recoverable } ->
                case recoverable of
                    BackendTask.Http.BadStatus metadata text ->
                        BackendTask.succeed (toResponse url metadata (Encode.string text))

                    BackendTask.Http.Timeout ->
                        BackendTask.fail (Transport { retryable = True, problem = "timed out" })

                    BackendTask.Http.NetworkError ->
                        BackendTask.fail (Transport { retryable = True, problem = "network error" })

                    BackendTask.Http.BadUrl bad ->
                        BackendTask.fail (Transport { retryable = False, problem = "bad URL " ++ bad })

                    BackendTask.Http.BadBody _ text ->
                        BackendTask.fail (BadBody text)
            )


toResponse : String -> BackendTask.Http.Metadata -> Decode.Value -> Response
toResponse url metadata body =
    { url = url
    , status = metadata.statusCode
    , headers = metadata.headers |> Dict.foldl (\k v acc -> Dict.insert (String.toLower k) v acc) Dict.empty
    , body = body
    }


{-| Header names are case-insensitive.
-}
header : String -> Response -> Maybe String
header name response =
    Dict.get (String.toLower name) response.headers


{-| The library version a response describes.
-}
lastModifiedVersion : Response -> Maybe Int
lastModifiedVersion response =
    header "last-modified-version" response |> Maybe.andThen String.toInt


note : Bool -> String -> BackendTask error ()
note enabled message =
    if enabled then
        Script.log message

    else
        BackendTask.succeed ()
