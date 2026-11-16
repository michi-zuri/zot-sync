module SchemaTest exposing (suite)

import Expect
import Fixtures
import Json.Decode as Decode
import Test exposing (Test, describe, test)
import Zotero.Schema as Schema exposing (Schema)


decodeFixture : String -> Result String Schema
decodeFixture json =
    Decode.decodeString Schema.decoder json |> Result.mapError Decode.errorToString


withSchema : String -> (Schema -> Expect.Expectation) -> Expect.Expectation
withSchema json check =
    case decodeFixture json of
        Ok schema ->
            check schema

        Err error ->
            Expect.fail error


fieldsOf : String -> Schema -> List String
fieldsOf typeName schema =
    schema.itemTypes
        |> List.filter (\t -> t.name == typeName)
        |> List.concatMap (.fields >> List.map .name)


suite : Test
suite =
    describe "Zotero.Schema"
        [ test "decodes the live schema (v42, 40 item types)" <|
            \() ->
                withSchema Fixtures.schemaV42
                    (\schema ->
                        ( schema.version, List.length schema.itemTypes )
                            |> Expect.equal ( 42, 40 )
                    )
        , test "keeps field order and base-field mappings (case.caseName -> title, case.court -> authority)" <|
            \() ->
                withSchema Fixtures.schemaV42
                    (\schema ->
                        schema.itemTypes
                            |> List.filter (\t -> t.name == "case")
                            |> List.concatMap .fields
                            |> List.take 3
                            |> Expect.equal
                                [ { name = "caseName", baseField = Just "title" }
                                , { name = "abstractNote", baseField = Nothing }
                                , { name = "court", baseField = Just "authority" }
                                ]
                    )
        , test "notes and annotations have no schema fields; attachments only three" <|
            \() ->
                withSchema Fixtures.schemaV42
                    (\schema ->
                        [ fieldsOf "note" schema, fieldsOf "annotation" schema, fieldsOf "attachment" schema ]
                            |> Expect.equal [ [], [], [ "title", "accessDate", "url" ] ]
                    )
        , test "the v32 -> v42 history really inserted podcast.date mid-list" <|
            \() ->
                Result.map2 (\old new -> ( fieldsOf "podcast" old |> List.take 4, fieldsOf "podcast" new |> List.take 6 ))
                    (decodeFixture Fixtures.schemaV32)
                    (decodeFixture Fixtures.schemaV42)
                    |> Expect.equal
                        (Ok
                            ( [ "title", "abstractNote", "seriesTitle", "episodeNumber" ]
                            , [ "title", "abstractNote", "seriesTitle", "episodeNumber", "audioFileType", "date" ]
                            )
                        )
        , test "baseFields lists generic names only, sorted" <|
            \() ->
                withSchema Fixtures.schemaV42
                    (\schema ->
                        let
                            bases =
                                Schema.baseFields schema
                        in
                        Expect.all
                            [ \b -> List.member "title" b |> Expect.equal True
                            , \b -> List.member "caseName" b |> Expect.equal False
                            , \b -> b |> Expect.equal (List.sort b)
                            ]
                            bases
                    )
        , test "fieldNamesForBase: generic name first, then every type-specific alias" <|
            \() ->
                withSchema Fixtures.schemaV42
                    (\schema ->
                        let
                            titles =
                                Schema.fieldNamesForBase schema "title"
                        in
                        Expect.all
                            [ \t -> List.head t |> Expect.equal (Just "title")
                            , \t -> List.member "caseName" t |> Expect.equal True
                            , \t -> List.member "nameOfAct" t |> Expect.equal True
                            , \t -> List.member "subject" t |> Expect.equal True
                            ]
                            titles
                    )
        , test "unknown entries are skipped, not fatal" <|
            \() ->
                """{"version": 99, "itemTypes": [
                    {"itemType": "book", "fields": [{"field": "title"}, {"nonsense": true}, {"field": "date", "baseField": 7}]},
                    {"noItemType": true},
                    {"itemType": "futureThing"}
                ]}"""
                    |> decodeFixture
                    |> Result.map (.itemTypes >> List.map (\t -> ( t.name, List.map .name t.fields )))
                    |> Expect.equal (Ok [ ( "book", [ "title", "date" ] ), ( "futureThing", [] ) ])
        ]
