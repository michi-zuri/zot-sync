module CliTest exposing (suite)

import BackendTask
import Expect
import FatalError
import Fixtures
import Json.Decode as Decode
import Json.Encode as Encode
import Sync
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest
import ZotSync exposing (Selection(..))
import Zotero.Api as Api
import Zotero.Library exposing (Library(..))


options : ZotSync.FetchOptions
options =
    { db = Just "postgres://localhost/zotero"
    , user = Nothing
    , group = Nothing
    , key = Nothing
    , all = False
    , skip = False
    , skipFulltext = False
    , verbose = False
    , apiUrl = Api.defaultBaseUrl
    }


fixture : String -> Encode.Value
fixture json =
    Decode.decodeString Decode.value json |> Result.withDefault Encode.null


rows : List Encode.Value -> Encode.Value
rows values =
    Encode.object [ ( "ok", Encode.bool True ), ( "rows", Encode.list identity values ), ( "rowCount", Encode.int (List.length values) ) ]


{-| The database's answer to Db.Layout.inspect: PostgreSQL 16, no 0.3 tables.
-}
inspected : Encode.Value
inspected =
    inspectedWith []


inspectedWith : List String -> Encode.Value
inspectedWith legacy =
    rows [ Encode.object [ ( "server_version", Encode.int 160004 ), ( "legacy_tables", Encode.list Encode.string legacy ) ] ]


committed : Encode.Value
committed =
    Encode.object [ ( "ok", Encode.bool True ), ( "results", Encode.list identity [] ) ]


respond url status body =
    BackendTaskTest.simulateHttp { method = "GET", url = url } { statusCode = status, statusText = "", headers = [], body = body }


suite : Test
suite =
    describe "ZotSync CLI"
        [ describe "which libraries (zot-sync 0.3 precedence)"
            [ test "--all with --user: the user's library and groups" <|
                \() ->
                    ZotSync.selection { options | user = Just 1, all = True }
                        |> Expect.equal (UserAndGroups { userId = Just 1, includeUser = True })
            , test "--all with --key and --skip: only the groups" <|
                \() ->
                    ZotSync.selection { options | key = Just "K", all = True, skip = True }
                        |> Expect.equal (UserAndGroups { userId = Nothing, includeUser = False })
            , test "--all alone means nothing extra" <|
                \() ->
                    ZotSync.selection { options | group = Just 2, all = True }
                        |> Expect.equal (OneGroup 2)
            , test "--user wins over --group and --key" <|
                \() ->
                    ZotSync.selection { options | user = Just 1, group = Just 2, key = Just "K" }
                        |> Expect.equal (OneUser 1)
            , test "--group wins over --key" <|
                \() ->
                    ZotSync.selection { options | group = Just 2, key = Just "K" }
                        |> Expect.equal (OneGroup 2)
            , test "--key alone: what the key can read, --skip drops the user library" <|
                \() ->
                    ( ZotSync.selection { options | key = Just "K" }, ZotSync.selection { options | key = Just "K", skip = True } )
                        |> Expect.equal ( KeyLibraries { includeUser = True }, KeyLibraries { includeUser = False } )
            , test "nothing" <|
                \() -> ZotSync.selection options |> Expect.equal NothingSelected
            ]
        , describe "discovering libraries"
            [ test "a key with access to all groups lists the user's groups" <|
                \() ->
                    ZotSync.targetsFor { api = { baseUrl = Api.defaultBaseUrl, apiKey = Just "K" }, verbose = False } (KeyLibraries { includeUser = True })
                        |> BackendTask.map (List.map (\t -> ( t.library, t.name )))
                        |> BackendTask.mapError (Sync.errorToString >> FatalError.fromString)
                        |> BackendTaskTest.fromBackendTask
                        |> respond "https://api.zotero.org/keys/current" 200 (fixture Fixtures.keyInfoAllGroups)
                        |> respond "https://api.zotero.org/users/1234567/groups?format=versions" 200 (Encode.object [ ( "2345678", Encode.int 7 ), ( "11", Encode.int 1 ) ])
                        |> BackendTaskTest.expectSuccessWith
                            (Expect.equal [ ( User 1234567, Just "zot-sync-tester" ), ( Group 11, Nothing ), ( Group 2345678, Nothing ) ])
            , test "a key limited to some groups and no personal library" <|
                \() ->
                    ZotSync.targetsFor { api = { baseUrl = Api.defaultBaseUrl, apiKey = Just "K" }, verbose = False } (KeyLibraries { includeUser = True })
                        |> BackendTask.map (List.map .library)
                        |> BackendTask.mapError (Sync.errorToString >> FatalError.fromString)
                        |> BackendTaskTest.fromBackendTask
                        |> respond "https://api.zotero.org/keys/current" 200 (fixture Fixtures.keyInfoSomeGroups)
                        |> BackendTaskTest.expectSuccessWith (Expect.equal [ Group 2345678 ])
            , test "a rejected key says so" <|
                \() ->
                    ZotSync.targetsFor { api = { baseUrl = Api.defaultBaseUrl, apiKey = Just "BAD" }, verbose = False } (KeyLibraries { includeUser = True })
                        |> BackendTask.mapError (Sync.errorToString >> FatalError.fromString)
                        |> BackendTaskTest.fromBackendTask
                        |> respond "https://api.zotero.org/keys/current" 403 (Encode.string "Forbidden")
                        |> BackendTaskTest.expectFailureWith (\e -> String.contains "rejected the API key" e.body |> Expect.equal True)
            ]
        , describe "settings from the environment"
            [ test "a flag wins over its variable; empty values count as unset" <|
                \() ->
                    [ ZotSync.withEnvironment { db = Just "postgres://env", apiKey = Just "ENVKEY" } { db = Nothing, key = Nothing }
                    , ZotSync.withEnvironment { db = Just "postgres://env", apiKey = Just "ENVKEY" } { db = Just "postgres://flag", key = Just "FLAGKEY" }
                    , ZotSync.withEnvironment { db = Just "", apiKey = Just "  " } { db = Nothing, key = Nothing }
                    , ZotSync.withEnvironment { db = Just "postgres://env", apiKey = Nothing } { db = Just "", key = Nothing }
                    ]
                        |> Expect.equal
                            [ { db = Just "postgres://env", key = Just "ENVKEY" }
                            , { db = Just "postgres://flag", key = Just "FLAGKEY" }
                            , { db = Nothing, key = Nothing }
                            , { db = Just "postgres://env", key = Nothing }
                            ]
            , test "with ZOT_SYNC_DB and ZOTERO_API_KEY set, fetch alone syncs what the key can read" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withEnv "ZOT_SYNC_DB" "postgres://from-env"
                                |> BackendTaskTest.withEnv "ZOTERO_API_KEY" "ENVKEY"
                            )
                            [ "fetch" ]
                        |> BackendTaskTest.ensureCustom "pgQuery"
                            (Decode.decodeValue (Decode.field "db" Decode.string) >> Expect.equal (Ok "postgres://from-env"))
                        |> BackendTaskTest.simulateCustom "pgQuery" inspected
                        |> BackendTaskTest.simulateCustom "pgTransaction" committed
                        -- the key from the environment is used: key-based discovery starts
                        |> respond "https://api.zotero.org/keys/current" 403 (Encode.string "Forbidden")
                        |> BackendTaskTest.expectFailureWith (\e -> String.contains "rejected the API key" e.body |> Expect.equal True)
            , test "--db on the command line wins over ZOT_SYNC_DB" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith (BackendTaskTest.init |> BackendTaskTest.withEnv "ZOT_SYNC_DB" "postgres://from-env")
                            [ "fetch", "--db", "postgres://from-flag" ]
                        |> BackendTaskTest.ensureCustom "pgQuery"
                            (Decode.decodeValue (Decode.field "db" Decode.string) >> Expect.equal (Ok "postgres://from-flag"))
                        |> BackendTaskTest.simulateCustom "pgQuery" inspected
                        |> BackendTaskTest.simulateCustom "pgTransaction" committed
                        |> BackendTaskTest.ensureStdout [ "The database is fine. Now say what to sync: --user, --group, or --key or ZOTERO_API_KEY (see zot-sync fetch --help)." ]
                        |> BackendTaskTest.expectSuccess
            , test "an empty ZOTERO_API_KEY is no key at all" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withEnv "ZOT_SYNC_DB" "postgres://from-env"
                                |> BackendTaskTest.withEnv "ZOTERO_API_KEY" ""
                            )
                            [ "fetch" ]
                        |> BackendTaskTest.simulateCustom "pgQuery" inspected
                        |> BackendTaskTest.simulateCustom "pgTransaction" committed
                        |> BackendTaskTest.ensureStdout [ "The database is fine. Now say what to sync: --user, --group, or --key or ZOTERO_API_KEY (see zot-sync fetch --help)." ]
                        |> BackendTaskTest.expectSuccess
            , test "check uses ZOTERO_API_KEY too" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith (BackendTaskTest.init |> BackendTaskTest.withEnv "ZOTERO_API_KEY" "ENVKEY") [ "check" ]
                        |> respond "https://api.zotero.org/keys/current" 200 (fixture Fixtures.keyInfoAllGroups)
                        |> BackendTaskTest.ensureOutputWith (List.length >> Expect.equal 1)
                        |> BackendTaskTest.expectSuccess
            ]
        , describe "running the script"
            [ test "fetch needs a database" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--user", "1" ]
                        |> BackendTaskTest.expectFailureWith (\e -> e.body |> Expect.equal "Say which database to sync into: pass --db or set ZOT_SYNC_DB.")
            , test "library ids must be numbers" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--db", "postgres://x", "--user", "me" ]
                        |> BackendTaskTest.expectTestError (String.contains "must be a numeric library id" >> Expect.equal True)
            , test "fetch with nothing selected checks the database and explains what to pass" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--db", "postgres://x" ]
                        |> BackendTaskTest.ensureCustom "pgQuery"
                            (Decode.decodeValue (Decode.field "db" Decode.string) >> Expect.equal (Ok "postgres://x"))
                        |> BackendTaskTest.simulateCustom "pgQuery" inspected
                        |> BackendTaskTest.simulateCustom "pgTransaction" committed
                        |> BackendTaskTest.ensureStdout [ "The database is fine. Now say what to sync: --user, --group, or --key or ZOTERO_API_KEY (see zot-sync fetch --help)." ]
                        |> BackendTaskTest.expectSuccess
            , test "a zot-sync 0.3 database is refused before anything is written" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--db", "postgres://x", "--user", "1" ]
                        |> BackendTaskTest.simulateCustom "pgQuery" (inspectedWith [ "logs.zot_fetch" ])
                        |> BackendTaskTest.expectFailureWith (\e -> String.contains "zot-sync 0.3 layout (logs.zot_fetch" e.body |> Expect.equal True)
            , test "check needs something to check" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "check" ]
                        |> BackendTaskTest.expectFailureWith (\e -> e.body |> Expect.equal "Nothing to check: pass --db and/or --key, or set ZOT_SYNC_DB and/or ZOTERO_API_KEY.")
            , test "check --key prints what the key can read, without the key" <|
                \() ->
                    ZotSync.run
                        |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "check", "--key", "K" ]
                        |> respond "https://api.zotero.org/keys/current" 200 (fixture Fixtures.keyInfoAllGroups)
                        |> BackendTaskTest.ensureOutputWith
                            (\outputs ->
                                case outputs of
                                    [ BackendTaskTest.Stdout json ] ->
                                        ( String.contains "\"userID\": 1234567" json, String.contains "REDACTED" json )
                                            |> Expect.equal ( True, False )

                                    _ ->
                                        Expect.fail "expected one line of output"
                            )
                        |> BackendTaskTest.expectSuccess
            ]
        , test "outcome lines" <|
            \() ->
                [ ZotSync.outcomeLine { library = User 1, name = Nothing }
                    (Ok { library = User 1, name = Just "Mine", fromVersion = 0, toVersion = 126, changed = True, viewsUpdated = True, stats = { items = 11, collections = 1, searches = 0, settings = 2, fulltext = 0, deleted = 0 } })
                , ZotSync.outcomeLine { library = Group 2, name = Nothing }
                    (Ok { library = Group 2, name = Nothing, fromVersion = 99, toVersion = 99, changed = False, viewsUpdated = False, stats = { items = 0, collections = 0, searches = 0, settings = 0, fulltext = 0, deleted = 0 } })
                , ZotSync.outcomeLine { library = Group 3, name = Nothing } (Err Sync.KeyRejected)
                ]
                    |> Expect.equal
                        [ "zot_u_1 \"Mine\": version 0 -> 126 · 11 items, 1 collection, 2 settings · views updated"
                        , "zot_g_2: up to date at version 99"
                        , "zot_g_3: FAILED: Zotero rejected the API key (HTTP 403); check it at https://www.zotero.org/settings/keys"
                        ]
        ]
