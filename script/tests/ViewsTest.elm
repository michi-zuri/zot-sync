module ViewsTest exposing (suite)

import Db.Sql exposing (Statement)
import Db.Views as Views exposing (ExistingColumn, SqlType(..))
import Dict exposing (Dict)
import Expect
import Fixtures
import Json.Decode as Decode
import Regex
import Test exposing (Test, describe, test)
import Zotero.Library exposing (Library(..))
import Zotero.Schema as Schema exposing (Schema)


schema : String -> Schema
schema json =
    Decode.decodeString Schema.decoder json
        |> Result.withDefault { version = -1, itemTypes = [] }


v32 : Schema
v32 =
    schema Fixtures.schemaV32


v42 : Schema
v42 =
    schema Fixtures.schemaV42


input : Schema -> Dict String (List String) -> Dict String (List ExistingColumn) -> Views.Input
input s observed existing =
    { library = User 1, schema = s, observed = observed, existing = existing }


{-| What the catalog would report after a fresh plan of these inputs ran.
-}
catalogAfter : Views.Input -> Dict String (List ExistingColumn)
catalogAfter i =
    Views.specs i
        |> List.map (\v -> ( v.name, List.map (\( n, t ) -> { name = n, sqlType = Just t }) v.columns ))
        |> Dict.fromList


columnsOf : Statement -> List String
columnsOf s =
    Regex.find (Maybe.withDefault Regex.never (Regex.fromString "\\)::[a-z]+ AS \"((?:[^\"]|\"\")*)\"")) s.sql
        |> List.filterMap (.submatches >> List.head >> Maybe.andThen identity)


viewName : Statement -> Maybe String
viewName s =
    Regex.find (Maybe.withDefault Regex.never (Regex.fromString "VIEW \"zot_u_1\"\\.\"([^\"]+)\"")) s.sql
        |> List.head
        |> Maybe.andThen (.submatches >> List.head >> Maybe.andThen identity)


statementFor : String -> List Statement -> List Statement
statementFor name =
    List.filter (\s -> viewName s == Just name)


specColumns : String -> Views.Input -> List String
specColumns name i =
    Views.specs i |> List.filter (\v -> v.name == name) |> List.concatMap (.columns >> List.map Tuple.first)


suite : Test
suite =
    describe "Db.Views"
        [ describe "fresh library"
            [ test "one schema, the items view, a view per item type, six auxiliary views" <|
                \() ->
                    Views.plan (input v42 Dict.empty Dict.empty)
                        |> List.length
                        |> Expect.equal (1 + 1 + 40 + 6)
            , test "type views keep zot-sync 0.3's column names and order (case view)" <|
                \() ->
                    specColumns "case" (input v42 Dict.empty Dict.empty)
                        |> List.take 18
                        |> Expect.equal
                            [ "key"
                            , "version"
                            , "numChildren"
                            , "dateAdded"
                            , "dateModified"
                            , "createdByUser"
                            , "lastModifiedByUser"
                            , "parsedDate"
                            , "creatorSummary"
                            , "itemType"
                            , "deleted"
                            , "creators"
                            , "tags"
                            , "collections"
                            , "relations"
                            , "caseName"
                            , "abstractNote"
                            , "court"
                            ]
            , test "the note view drops creator columns and adds note, customJSON, parentItem" <|
                \() ->
                    specColumns "note" (input v42 Dict.empty Dict.empty)
                        |> Expect.equal
                            [ "key"
                            , "version"
                            , "numChildren"
                            , "dateAdded"
                            , "dateModified"
                            , "createdByUser"
                            , "lastModifiedByUser"
                            , "itemType"
                            , "deleted"
                            , "tags"
                            , "collections"
                            , "relations"
                            , "note"
                            , "customJSON"
                            , "parentItem"
                            ]
            , test "annotations get their non-schema fields and a typed position" <|
                \() ->
                    Views.specs (input v42 Dict.empty Dict.empty)
                        |> List.filter (\v -> v.name == "annotation")
                        |> List.concatMap .columns
                        |> List.filter (\( n, _ ) -> String.startsWith "annotation" n || n == "parentItem")
                        |> Expect.equal
                            [ ( "annotationType", Text )
                            , ( "annotationAuthorName", Text )
                            , ( "annotationText", Text )
                            , ( "annotationComment", Text )
                            , ( "annotationColor", Text )
                            , ( "annotationPageLabel", Text )
                            , ( "annotationSortIndex", Text )
                            , ( "annotationPosition", Jsonb )
                            , ( "parentItem", Text )
                            ]
            , test "fields seen in the data but unknown to zot-sync are appended, sorted" <|
                \() ->
                    specColumns "annotation" (input v42 (Dict.fromList [ ( "annotation", [ "annotationText", "zNew", "aNew", "parentItem" ] ) ]) Dict.empty)
                        |> List.reverse
                        |> List.take 3
                        |> Expect.equal [ "zNew", "aNew", "parentItem" ]
            , test "item types the schema does not know yet still get a view" <|
                \() ->
                    Views.specs (input v42 (Dict.fromList [ ( "hologram", [ "title", "depth" ] ) ]) Dict.empty)
                        |> List.filter (\v -> v.name == "hologram")
                        |> List.concatMap (.columns >> List.map Tuple.first)
                        |> List.reverse
                        |> List.take 2
                        |> Expect.equal [ "title", "depth" ]
            , test "the items view has every generic field, sorted like 0.3, then child-item fields" <|
                \() ->
                    let
                        cols =
                            specColumns "items" (input v42 Dict.empty Dict.empty)
                    in
                    Expect.all
                        [ \c -> List.take 2 c |> Expect.equal [ "key", "version" ]
                        , \c -> List.member "caseName" c |> Expect.equal False
                        , \c -> List.member "title" c |> Expect.equal True
                        , \c -> List.member "customJSON" c |> Expect.equal True
                        , \c -> List.member "annotationText" c |> Expect.equal True
                        , \c -> List.member "linkMode" c |> Expect.equal True
                        ]
                        cols
            , test "items.authority covers every alias, e.g. case.court (a base-field remap from v22)" <|
                \() ->
                    Views.plan (input v42 Dict.empty Dict.empty)
                        |> statementFor "items"
                        |> List.map (.sql >> String.contains "(NULLIF(COALESCE(i.data->>'court', i.data->>'issuingAuthority', i.data->>'legislativeBody', i.data->>'organization'), ''))::text AS \"authority\"")
                        |> Expect.equal [ True ]
            , test "items.title shows a note's HTML, as 0.3 stored it" <|
                \() ->
                    Views.plan (input v42 Dict.empty Dict.empty)
                        |> statementFor "items"
                        |> List.map (.sql >> String.contains "CASE WHEN i.item_type = 'note' THEN i.data->>'note' ELSE COALESCE(i.data->>'title'")
                        |> Expect.equal [ True ]
            ]
        , describe "schema upgrade v32 -> v42 (the change that broke zot-sync 0.3)"
            [ test "every existing view is replaced in place: old columns keep their position, new ones are appended" <|
                \() ->
                    let
                        before =
                            catalogAfter (input v32 Dict.empty Dict.empty)

                        statements =
                            Views.plan (input v42 Dict.empty before)

                        check ( name, existing ) =
                            case statementFor name statements of
                                [ s ] ->
                                    let
                                        cols =
                                            columnsOf s

                                        oldNames =
                                            List.map .name existing
                                    in
                                    String.startsWith "CREATE OR REPLACE VIEW" s.sql
                                        && (List.take (List.length oldNames) cols == oldNames)

                                _ ->
                                    False
                    in
                    Dict.toList before
                        |> List.filter (check >> not)
                        |> List.map Tuple.first
                        |> Expect.equal []
            , test "podcast.date, inserted mid-list by Zotero, lands at the end of the view" <|
                \() ->
                    Views.plan (input v42 Dict.empty (catalogAfter (input v32 Dict.empty Dict.empty)))
                        |> statementFor "podcast"
                        |> List.concatMap columnsOf
                        |> List.drop (List.length (specColumns "podcast" (input v32 Dict.empty Dict.empty)))
                        |> List.member "date"
                        |> Expect.equal True
            , test "a column whose field disappeared stays, still reading the data" <|
                \() ->
                    let
                        existing =
                            Dict.fromList [ ( "book", [ { name = "key", sqlType = Just Text }, { name = "oldField", sqlType = Just Text } ] ) ]
                    in
                    Views.plan (input v42 Dict.empty existing)
                        |> statementFor "book"
                        |> List.map (\s -> ( List.take 2 (columnsOf s), String.contains "(NULLIF(i.data->>'oldField', ''))::text AS \"oldField\"" s.sql ))
                        |> Expect.equal [ ( [ "key", "oldField" ], True ) ]
            , test "replanning an up-to-date library only replaces views, never drops them" <|
                \() ->
                    let
                        current =
                            catalogAfter (input v42 Dict.empty Dict.empty)
                    in
                    Views.plan (input v42 Dict.empty current)
                        |> List.filter (\s -> String.startsWith "DROP" s.sql || String.startsWith "CREATE VIEW" s.sql)
                        |> Expect.equal []
            , test "a changed column type (only a zot-sync upgrade can cause it) drops and recreates that view" <|
                \() ->
                    let
                        existing =
                            Dict.fromList [ ( "book", [ { name = "key", sqlType = Just Text }, { name = "version", sqlType = Just Text } ] ) ]
                    in
                    Views.plan (input v42 Dict.empty existing)
                        |> List.filter (\s -> viewName s == Just "book")
                        |> List.map (.sql >> String.words >> List.take 2 >> String.join " ")
                        |> Expect.equal [ "DROP VIEW", "CREATE VIEW" ]
            , test "an unrecognised column type also recreates the view" <|
                \() ->
                    Views.plan (input v42 Dict.empty (Dict.fromList [ ( "book", [ { name = "key", sqlType = Nothing } ] ) ]))
                        |> statementFor "book"
                        |> List.length
                        |> Expect.equal 2
            ]
        , describe "safety"
            [ test "hostile field names are quoted as identifiers and literals" <|
                \() ->
                    Views.plan (input v42 (Dict.fromList [ ( "book", [ "x\"'); DROP TABLE zotero.item; --" ] ) ]) Dict.empty)
                        |> statementFor "book"
                        |> List.map (.sql >> String.contains "(NULLIF(i.data->>'x\"''); DROP TABLE zotero.item; --', ''))::text AS \"x\"\"'); DROP TABLE zotero.item; --\"")
                        |> Expect.equal [ True ]
            , test "names are cut to Postgres' 63-byte limit before deduplication" <|
                \() ->
                    specColumns "book" (input v42 (Dict.fromList [ ( "book", [ String.repeat 70 "a", String.repeat 64 "a" ] ) ]) Dict.empty)
                        |> List.filter (String.startsWith "aaaa")
                        |> Expect.equal [ String.repeat 63 "a" ]
            , test "catalog rows are grouped per view in column order" <|
                \() ->
                    Views.groupExisting
                        [ ( "a", { name = "x", sqlType = Just Text } )
                        , ( "a", { name = "y", sqlType = Just Integer } )
                        , ( "b", { name = "z", sqlType = Nothing } )
                        ]
                        |> Dict.map (\_ cs -> List.map .name cs)
                        |> Expect.equal (Dict.fromList [ ( "a", [ "x", "y" ] ), ( "b", [ "z" ] ) ])
            ]
        ]
