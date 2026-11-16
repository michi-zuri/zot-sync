module HttpTest exposing (suite)

import BackendTask
import Dict
import Expect
import FatalError
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest exposing (HttpError(..))
import Zotero.Api as Api exposing (Kind(..))
import Zotero.Http as Http
import Zotero.Library exposing (Library(..))


url : String
url =
    "https://api.zotero.org/users/1/settings"


respond status headers body =
    BackendTaskTest.simulateHttp { method = "GET", url = url } { statusCode = status, statusText = "", headers = headers, body = body }


getting =
    Http.get False url []
        |> BackendTask.map (\r -> ( r.status, Http.lastModifiedVersion r ))
        |> BackendTask.mapError (Http.errorToString >> FatalError.fromString)
        |> BackendTaskTest.fromBackendTask


suite : Test
suite =
    describe "Zotero.Http and Zotero.Api"
        [ describe "flow control"
            [ test "a 2xx response comes back with lower-cased headers" <|
                \() ->
                    getting
                        |> respond 200 [ ( "Last-Modified-Version", "42" ) ] (Encode.object [])
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 200, Just 42 ))
            , test "304 is returned, not treated as an error" <|
                \() ->
                    getting
                        |> respond 304 [ ( "Last-Modified-Version", "42" ) ] Encode.null
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 304, Just 42 ))
            , test "429 with Retry-After waits that long, then retries" <|
                \() ->
                    getting
                        |> respond 429 [ ( "Retry-After", "7" ) ] (Encode.string "Too many requests")
                        |> BackendTaskTest.ensureStdout [ "Zotero asked to slow down (HTTP 429); waiting 7s" ]
                        |> respond 200 [ ( "Last-Modified-Version", "3" ) ] (Encode.object [])
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 200, Just 3 ))
            , test "503 with Retry-After is retried too" <|
                \() ->
                    getting
                        |> respond 503 [ ( "retry-after", "1" ) ] (Encode.string "Service unavailable")
                        |> respond 200 [] (Encode.object [])
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 200, Nothing ))
            , test "503 without Retry-After is returned to the caller" <|
                \() ->
                    getting
                        |> respond 503 [] (Encode.string "down")
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 503, Nothing ))
            , test "after maxAttempts rate limits, the last answer is returned" <|
                \() ->
                    List.foldl (\_ t -> respond 429 [] (Encode.string "slow down") t) getting (List.range 1 Http.maxAttempts)
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 429, Nothing ))
            , test "a Backoff header pauses before the response is used" <|
                \() ->
                    getting
                        |> respond 200 [ ( "Backoff", "30" ) ] (Encode.object [])
                        |> BackendTaskTest.ensureStdout [ "Zotero asked clients to back off; pausing 30s" ]
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 200, Nothing ))
            , test "network errors are retried" <|
                \() ->
                    getting
                        |> BackendTaskTest.simulateHttpError "GET" url NetworkError
                        |> BackendTaskTest.simulateHttpError "GET" url Timeout
                        |> respond 200 [] (Encode.object [])
                        |> BackendTaskTest.expectSuccessWith (Expect.equal ( 200, Nothing ))
            , test "persistent network errors fail with the URL in the message" <|
                \() ->
                    List.foldl (\_ t -> BackendTaskTest.simulateHttpError "GET" url NetworkError t) getting (List.range 1 Http.maxAttempts)
                        |> BackendTaskTest.expectFailureWith
                            (\e -> e.body |> String.contains ("could not reach " ++ url) |> Expect.equal True)
            ]
        , describe "URLs (as the official client builds them)"
            [ test "versions of changed items include the trash" <|
                \() ->
                    Api.versionsUrl config (User 1) Items 41
                        |> Expect.equal "https://api.zotero.org/users/1/items?format=versions&includeTrashed=1&since=41"
            , test "a full listing leaves out since" <|
                \() ->
                    Api.versionsUrl config (Group 2) Collections 0
                        |> Expect.equal "https://api.zotero.org/groups/2/collections?format=versions"
            , test "objects by key, with an explicit limit" <|
                \() ->
                    Api.objectsUrl config (User 1) Items [ "AAAAAAAA", "BBBBBBBB" ]
                        |> Expect.equal "https://api.zotero.org/users/1/items?itemKey=AAAAAAAA%2CBBBBBBBB&includeTrashed=1&limit=100"
            , test "searches by key" <|
                \() ->
                    Api.objectsUrl config (User 1) Searches [ "SA2B3C4D" ]
                        |> Expect.equal "https://api.zotero.org/users/1/searches?searchKey=SA2B3C4D&limit=100"
            , test "deleted, settings, full text, groups, keys, schema" <|
                \() ->
                    [ Api.deletedUrl config (User 1) 5
                    , Api.settingsUrl config (User 1) 5
                    , Api.fulltextVersionsUrl config (User 1) 5
                    , Api.fulltextUrl config (User 1) "HI9J2K3L"
                    , Api.userGroupsUrl config 1
                    , Api.groupUrl config 2
                    , Api.keyInfoUrl config
                    , Api.schemaUrl { config | baseUrl = "http://127.0.0.1:8080/" }
                    ]
                        |> Expect.equal
                            [ "https://api.zotero.org/users/1/deleted?since=5"
                            , "https://api.zotero.org/users/1/settings?since=5"
                            , "https://api.zotero.org/users/1/fulltext?format=versions&since=5"
                            , "https://api.zotero.org/users/1/items/HI9J2K3L/fulltext"
                            , "https://api.zotero.org/users/1/groups?format=versions"
                            , "https://api.zotero.org/groups/2"
                            , "https://api.zotero.org/keys/current"
                            , "http://127.0.0.1:8080/schema"
                            ]
            , test "the key goes in a header, never in the URL" <|
                \() ->
                    ( Api.headers { config | apiKey = Just "SECRET" } |> Dict.fromList |> Dict.get "Zotero-API-Key"
                    , Api.headers config
                    )
                        |> Expect.equal ( Just "SECRET", [ ( "Zotero-API-Version", "3" ) ] )
            ]
        ]


config : Api.Config
config =
    { baseUrl = Api.defaultBaseUrl, apiKey = Nothing }
