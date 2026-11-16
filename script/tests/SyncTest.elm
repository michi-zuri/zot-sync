module SyncTest exposing (suite)

{-| Whole runs of the script with simulated Zotero responses and a simulated
database. Each test spells out, in order, every request and statement the run
makes, so these double as documentation of the protocol.
-}

import Expect
import Fixtures
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest
import ZotSync


fixture : String -> Encode.Value
fixture json =
    Decode.decodeString Decode.value json |> Result.withDefault Encode.null


rows : List Encode.Value -> Encode.Value
rows values =
    Encode.object [ ( "ok", Encode.bool True ), ( "rows", Encode.list identity values ), ( "rowCount", Encode.int (List.length values) ) ]


affected : Int -> Encode.Value
affected n =
    Encode.object [ ( "ok", Encode.bool True ), ( "rows", Encode.list identity [] ), ( "rowCount", Encode.int n ) ]


committed : Encode.Value
committed =
    Encode.object [ ( "ok", Encode.bool True ), ( "results", Encode.list identity [] ) ]


respond url status headers body =
    BackendTaskTest.simulateHttp { method = "GET", url = url } { statusCode = status, statusText = "", headers = headers, body = body }


sqlStartsWith : String -> Encode.Value -> Expect.Expectation
sqlStartsWith prefix input =
    case Decode.decodeValue (Decode.field "sql" Decode.string) input of
        Ok sql ->
            String.startsWith prefix sql |> Expect.equal True |> Expect.onFail ("SQL was: " ++ sql)

        Err _ ->
            Expect.fail "no sql in port input"


{-| The database answers for the start of every run: no 0.3 tables, the
layout transaction, and a cached schema that the API confirms with 304.
-}
preparedWithCachedSchema test_ =
    test_
        |> BackendTaskTest.simulateCustom "pgQuery" (rows [])
        |> BackendTaskTest.simulateCustom "pgTransaction" committed
        |> BackendTaskTest.ensureCustom "pgQuery" (sqlStartsWith "SELECT version, etag, document FROM zotero.schema")
        |> BackendTaskTest.simulateCustom "pgQuery"
            (rows [ Encode.object [ ( "version", Encode.int 42 ), ( "etag", Encode.string "\"s42\"" ), ( "document", fixture Fixtures.schemaV42 ) ] ])
        |> respond "https://api.zotero.org/schema" 304 [] Encode.null


{-| A library already synced to version 126 with views for this schema.
-}
upToDateLibrary runId test_ =
    test_
        |> BackendTaskTest.ensureCustom "pgQuery" (sqlStartsWith "INSERT INTO zotero.library")
        |> BackendTaskTest.simulateCustom "pgQuery" (affected 0)
        |> BackendTaskTest.simulateCustom "pgQuery" (rows [ Encode.object [ ( "version", Encode.int 126 ), ( "views_revision", Encode.string "views-1:\"s42\"" ) ] ])
        |> BackendTaskTest.ensureCustom "pgQuery" (sqlStartsWith "INSERT INTO zotero.sync_run")
        |> BackendTaskTest.simulateCustom "pgQuery" (rows [ Encode.object [ ( "id", Encode.int runId ) ] ])


suite : Test
suite =
    describe "whole runs"
        [ test "an unchanged library costs one request: settings answers 304" <|
            \() ->
                ZotSync.run
                    |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--db", "postgres://x", "--user", "1234567" ]
                    |> preparedWithCachedSchema
                    |> upToDateLibrary 7
                    |> respond "https://api.zotero.org/users/1234567/settings?since=126" 304 [ ( "Last-Modified-Version", "126" ) ] Encode.null
                    |> BackendTaskTest.ensureCustom "pgTransaction"
                        (Decode.decodeValue (Decode.field "statements" (Decode.list (Decode.field "sql" Decode.string)))
                            >> Result.map (List.map (String.words >> List.take 2 >> String.join " "))
                            >> Expect.equal (Ok [ "UPDATE zotero.library", "UPDATE zotero.sync_run" ])
                        )
                    |> BackendTaskTest.simulateCustom "pgTransaction" committed
                    |> BackendTaskTest.ensureStdout [ "zot_u_1234567: up to date at version 126" ]
                    |> BackendTaskTest.expectSuccess
        , test "a library that fails is recorded and reported, and the next one still syncs" <|
            \() ->
                ZotSync.run
                    |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--db", "postgres://x", "--key", "K", "--all" ]
                    |> BackendTaskTest.simulateCustom "pgQuery" (rows [])
                    |> BackendTaskTest.simulateCustom "pgTransaction" committed
                    |> respond "https://api.zotero.org/keys/current" 200 [] (fixture Fixtures.keyInfoAllGroups)
                    |> respond "https://api.zotero.org/users/1234567/groups?format=versions" 200 [] (Encode.object [ ( "2345678", Encode.int 7 ) ])
                    |> BackendTaskTest.simulateCustom "pgQuery"
                        (rows [ Encode.object [ ( "version", Encode.int 42 ), ( "etag", Encode.string "\"s42\"" ), ( "document", fixture Fixtures.schemaV42 ) ] ])
                    |> respond "https://api.zotero.org/schema" 304 [] Encode.null
                    -- the user library: up to date
                    |> upToDateLibrary 1
                    |> respond "https://api.zotero.org/users/1234567/settings?since=126" 304 [] Encode.null
                    |> BackendTaskTest.simulateCustom "pgTransaction" committed
                    |> BackendTaskTest.ensureStdout [ "zot_u_1234567 \"zot-sync-tester\": up to date at version 126" ]
                    -- the group: never synced, and its metadata is forbidden
                    |> BackendTaskTest.simulateCustom "pgQuery" (affected 1)
                    |> BackendTaskTest.simulateCustom "pgQuery" (rows [])
                    |> BackendTaskTest.simulateCustom "pgQuery" (rows [ Encode.object [ ( "id", Encode.int 2 ) ] ])
                    |> respond "https://api.zotero.org/groups/2345678" 403 [] (Encode.string "Forbidden")
                    |> BackendTaskTest.ensureCustom "pgQuery"
                        (\input ->
                            Decode.decodeValue (Decode.map2 Tuple.pair (Decode.field "sql" Decode.string) (Decode.field "params" (Decode.index 1 Decode.string))) input
                                |> Result.map (\( sql, message ) -> ( String.startsWith "UPDATE zotero.sync_run SET finished_at = now(), status = 'failed'" sql, message ))
                                |> Expect.equal (Ok ( True, "HTTP 403 from https://api.zotero.org/groups/2345678: Forbidden" ))
                                |> Expect.onFail (Encode.encode 0 input)
                        )
                    |> BackendTaskTest.simulateCustom "pgQuery" (affected 1)
                    |> BackendTaskTest.ensureStdout [ "zot_g_2345678: FAILED: HTTP 403 from https://api.zotero.org/groups/2345678: Forbidden" ]
                    |> BackendTaskTest.expectFailureWith
                        (\e -> e.body |> Expect.equal "1 of 2 libraries could not be synced (see above).")
        ]
