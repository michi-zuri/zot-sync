module LayoutTest exposing (suite)

{-| The safe casts behind the views (michi-zuri/zot-sync#1).
-}

import Db.Layout as Layout
import Db.Sql exposing (Statement)
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest
import ZotSync


castNames : List String
castNames =
    [ "try_jsonb", "try_integer", "try_bigint", "try_boolean", "try_timestamp", "try_timestamptz" ]


functionsIn : List Statement -> List String
functionsIn =
    List.map .sql >> List.filter (String.startsWith "CREATE OR REPLACE FUNCTION")


castFor : String -> List Statement -> List String
castFor name =
    functionsIn >> List.filter (String.contains ("FUNCTION zotero." ++ name ++ "("))


allContain : String -> List String -> Bool
allContain fragment =
    List.all (String.contains fragment)


suite : Test
suite =
    describe "Db.Layout safe casts"
        [ test "from PostgreSQL 16 they test the input with pg_input_is_valid, in plain SQL" <|
            \() ->
                castNames
                    |> List.concatMap (\name -> castFor name (Layout.setup 160000))
                    |> Expect.all
                        [ List.length >> Expect.equal 6
                        , allContain "LANGUAGE sql STABLE PARALLEL SAFE" >> Expect.equal True
                        , allContain "pg_input_is_valid(value, '" >> Expect.equal True
                        ]
        , test "each one checks the type it casts to" <|
            \() ->
                castFor "try_timestamptz" (Layout.setup 180001)
                    |> Expect.equal
                        [ "CREATE OR REPLACE FUNCTION zotero.try_timestamptz(value text) RETURNS timestamptz\nLANGUAGE sql STABLE PARALLEL SAFE AS\n$$ SELECT (CASE WHEN pg_input_is_valid(value, 'timestamptz') THEN value END)::timestamptz $$" ]
        , test "from PostgreSQL 16 nothing in the setup uses an exception handler" <|
            \() ->
                Layout.setup 160000
                    |> List.filter (.sql >> String.contains "EXCEPTION")
                    |> Expect.equal []
        , test "before PostgreSQL 16 they keep the exception handler, and are PARALLEL UNSAFE because of it" <|
            \() ->
                castNames
                    |> List.concatMap (\name -> castFor name (Layout.setup 159999))
                    |> Expect.all
                        [ List.length >> Expect.equal 6
                        , allContain "LANGUAGE plpgsql STABLE PARALLEL UNSAFE" >> Expect.equal True
                        , allContain "EXCEPTION WHEN others THEN RETURN NULL" >> Expect.equal True
                        ]
        , test "custom_json is STABLE everywhere, so it is inlined along with try_jsonb, and as parallel safe as its casts" <|
            \() ->
                [ 130000, 160000, 180001 ]
                    |> List.concatMap (\version -> castFor "custom_json" (Layout.setup version))
                    |> List.map (\sql -> ( String.contains "LANGUAGE sql STABLE" sql, String.contains "PARALLEL UNSAFE" sql ))
                    |> Expect.equal [ ( True, True ), ( True, False ), ( True, False ) ]
        , test "a run installs the definitions for the server it talks to" <|
            \() ->
                let
                    setupFor version expectedFragment =
                        ZotSync.run
                            |> BackendTaskTest.fromScriptWith BackendTaskTest.init [ "fetch", "--db", "postgres://x" ]
                            |> BackendTaskTest.ensureCustom "pgQuery" (Decode.decodeValue (Decode.field "sql" Decode.string) >> Result.map (String.contains "server_version_num") >> Expect.equal (Ok True))
                            |> BackendTaskTest.simulateCustom "pgQuery" (inspection version)
                            |> BackendTaskTest.ensureCustom "pgTransaction"
                                (Decode.decodeValue (Decode.field "statements" (Decode.list (Decode.field "sql" Decode.string)))
                                    >> Result.map (List.filter (String.contains "FUNCTION zotero.try_jsonb(") >> List.map (String.contains expectedFragment))
                                    >> Expect.equal (Ok [ True ])
                                )
                            |> BackendTaskTest.simulateCustom "pgTransaction" (Encode.object [ ( "ok", Encode.bool True ), ( "results", Encode.list identity [] ) ])
                            |> BackendTaskTest.expectSuccess
                in
                Expect.all
                    [ \() -> setupFor 150013 "EXCEPTION WHEN others"
                    , \() -> setupFor 180001 "pg_input_is_valid(value, 'jsonb')"
                    ]
                    ()
        ]


inspection : Int -> Encode.Value
inspection version =
    Encode.object
        [ ( "ok", Encode.bool True )
        , ( "rows", Encode.list identity [ Encode.object [ ( "server_version", Encode.int version ), ( "legacy_tables", Encode.list Encode.string [] ) ] ] )
        , ( "rowCount", Encode.int 1 )
        ]
