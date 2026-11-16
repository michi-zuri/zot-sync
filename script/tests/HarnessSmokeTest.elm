module HarnessSmokeTest exposing (suite)

{-| Proves the three effect patterns the sync engine relies on can be driven
from elm-test via Test.BackendTask:

1.  reading Zotero's `Last-Modified-Version` response header,
2.  treating `304 Not Modified` (answer to `If-Modified-Since-Version`) as data,
3.  a JSON round trip through a `BackendTask.Custom` port (our Postgres bridge).

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Http
import Dict exposing (Dict)
import Expect
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest


type Versions
    = Changed { libraryVersion : Int, versions : Dict String Int }
    | NotModified


versionsUrl : Int -> String
versionsUrl since =
    "https://api.zotero.org/users/1/items?format=versions&includeTrashed=1&since=" ++ String.fromInt since


fetchVersions : Int -> BackendTask FatalError Versions
fetchVersions since =
    BackendTask.Http.request
        { url = versionsUrl since
        , method = "GET"
        , headers =
            [ ( "Zotero-API-Version", "3" )
            , ( "If-Modified-Since-Version", String.fromInt since )
            ]
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        (BackendTask.Http.expectJson (Decode.dict Decode.int)
            |> BackendTask.Http.withMetadata
                (\meta versions ->
                    Changed
                        { libraryVersion =
                            header "last-modified-version" meta.headers
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault -1
                        , versions = versions
                        }
                )
        )
        |> BackendTask.onError
            (\{ fatal, recoverable } ->
                case recoverable of
                    BackendTask.Http.BadStatus meta _ ->
                        if meta.statusCode == 304 then
                            BackendTask.succeed NotModified

                        else
                            BackendTask.fail fatal

                    _ ->
                        BackendTask.fail fatal
            )


{-| HTTP header names are case-insensitive; never trust the casing we get.
-}
header : String -> Dict String String -> Maybe String
header name headers =
    headers
        |> Dict.toList
        |> List.filter (\( k, _ ) -> String.toLower k == name)
        |> List.head
        |> Maybe.map Tuple.second


query : String -> List Encode.Value -> BackendTask FatalError (List (Dict String Decode.Value))
query sql params =
    BackendTask.Custom.run "pgQuery"
        (Encode.object [ ( "sql", Encode.string sql ), ( "params", Encode.list identity params ) ])
        (Decode.field "rows" (Decode.list (Decode.dict Decode.value)))
        |> BackendTask.allowFatal


suite : Test
suite =
    describe "Test.BackendTask harness"
        [ test "reads Last-Modified-Version regardless of header casing" <|
            \() ->
                fetchVersions 10
                    |> BackendTaskTest.fromBackendTask
                    |> BackendTaskTest.simulateHttp { method = "GET", url = versionsUrl 10 }
                        { statusCode = 200
                        , statusText = "OK"
                        , headers = [ ( "Last-Modified-Version", "12" ) ]
                        , body = Encode.object [ ( "ABCD2345", Encode.int 12 ) ]
                        }
                    |> BackendTaskTest.expectSuccessWith
                        (Expect.equal
                            (Changed { libraryVersion = 12, versions = Dict.fromList [ ( "ABCD2345", 12 ) ] })
                        )
        , test "304 Not Modified is data, not an error" <|
            \() ->
                fetchVersions 12
                    |> BackendTaskTest.fromBackendTask
                    |> BackendTaskTest.simulateHttp { method = "GET", url = versionsUrl 12 }
                        { statusCode = 304
                        , statusText = "Not Modified"
                        , headers = [ ( "last-modified-version", "12" ) ]
                        , body = Encode.null
                        }
                    |> BackendTaskTest.expectSuccessWith (Expect.equal NotModified)
        , test "other statuses stay fatal" <|
            \() ->
                fetchVersions 12
                    |> BackendTaskTest.fromBackendTask
                    |> BackendTaskTest.simulateHttp { method = "GET", url = versionsUrl 12 }
                        { statusCode = 403, statusText = "Forbidden", headers = [], body = Encode.string "Forbidden" }
                    |> BackendTaskTest.expectFailure
        , test "custom Postgres port: assert on SQL sent, simulate rows returned" <|
            \() ->
                query "SELECT version FROM zotero.library WHERE id = $1" [ Encode.string "u1" ]
                    |> BackendTask.map (List.map (Dict.get "version" >> Maybe.map (Decode.decodeValue Decode.int)))
                    |> BackendTaskTest.fromBackendTask
                    |> BackendTaskTest.ensureCustom "pgQuery"
                        (Decode.decodeValue (Decode.field "sql" Decode.string)
                            >> Expect.equal (Ok "SELECT version FROM zotero.library WHERE id = $1")
                        )
                    |> BackendTaskTest.simulateCustom "pgQuery"
                        (Encode.object [ ( "rows", Encode.list identity [ Encode.object [ ( "version", Encode.int 41 ) ] ] ) ])
                    |> BackendTaskTest.expectSuccessWith (Expect.equal [ Just (Ok 41) ])
        ]
