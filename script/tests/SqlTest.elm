module SqlTest exposing (suite)

import Db.Sql as Sql
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Zotero.Library as Library exposing (Library(..))


{-| Undo identifier quoting: strip the outer quotes, collapse doubled quotes.
Fails if any quote inside is not doubled, i.e. if quoting could be escaped.
-}
unquote : Char -> String -> Maybe String
unquote q quoted =
    let
        inner =
            quoted |> String.dropLeft 1 |> String.dropRight 1

        q2 =
            String.fromList [ q, q ]
    in
    if String.startsWith (String.fromChar q) quoted && String.endsWith (String.fromChar q) quoted && not (String.contains (String.fromChar q) (String.replace q2 "" inner)) then
        Just (String.replace q2 (String.fromChar q) inner)

    else
        Nothing


suite : Test
suite =
    describe "Db.Sql quoting and library naming"
        [ test "identifiers keep camelCase by quoting" <|
            \() -> Sql.identifier "publicationTitle" |> Expect.equal "\"publicationTitle\""
        , test "embedded double quotes are doubled" <|
            \() -> Sql.identifier "a\"b" |> Expect.equal "\"a\"\"b\""
        , test "qualified names" <|
            \() -> Sql.qualified "zot_u_1" "journalArticle" |> Expect.equal "\"zot_u_1\".\"journalArticle\""
        , test "literals double single quotes" <|
            \() -> Sql.literal "x'); DROP TABLE items; --" |> Expect.equal "'x''); DROP TABLE items; --'"
        , fuzz Fuzz.string "identifier quoting always round-trips" <|
            \s -> Sql.identifier s |> unquote '"' |> Expect.equal (Just s)
        , fuzz Fuzz.string "literal quoting always round-trips" <|
            \s -> Sql.literal s |> unquote '\'' |> Expect.equal (Just s)
        , test "library ids and schema names match zot-sync 0.3" <|
            \() ->
                [ User 123, Group 456 ]
                    |> List.map (\l -> ( Library.id l, Library.schemaName l, Library.pathPrefix l ))
                    |> Expect.equal
                        [ ( "u123", "zot_u_123", [ "users", "123" ] )
                        , ( "g456", "zot_g_456", [ "groups", "456" ] )
                        ]
        ]
