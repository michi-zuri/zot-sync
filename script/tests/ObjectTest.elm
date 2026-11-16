module ObjectTest exposing (suite)

import Dict
import Expect
import Fixtures
import Json.Decode as Decode
import Test exposing (Test, describe, test)
import Zotero.Object as Object exposing (GroupAccess(..), Item)


items : String -> Result String (List Item)
items json =
    Decode.decodeString (Decode.list Object.itemDecoder) json |> Result.mapError Decode.errorToString


byKey : String -> List Item -> Maybe Item
byKey key =
    List.filter (\i -> i.key == key) >> List.head


dataField : String -> Decode.Decoder a -> Item -> Result Decode.Error a
dataField name decoder item =
    Decode.decodeValue (Decode.field name decoder) item.data


suite : Test
suite =
    describe "Zotero.Object"
        [ describe "items"
            [ test "top-level items decode with type, no parent" <|
                \() ->
                    items Fixtures.itemsTop
                        |> Result.map (List.map (\i -> ( i.key, i.itemType, i.parentKey )))
                        |> Expect.equal
                            (Ok
                                [ ( "AB2C3D4E", "journalArticle", Nothing )
                                , ( "BC3D4E5F", "book", Nothing )
                                , ( "CD4E5F6G", "case", Nothing )
                                , ( "DE5F6G7H", "note", Nothing )
                                , ( "EF6G7H8I", "attachment", Nothing )
                                , ( "FG7H8I9J", "podcast", Nothing )
                                ]
                            )
            , test "child notes, attachments and annotations keep their parent" <|
                \() ->
                    items Fixtures.itemsChildren
                        |> Result.map (List.map (\i -> ( i.itemType, i.parentKey )))
                        |> Expect.equal
                            (Ok
                                [ ( "note", Just "AB2C3D4E" )
                                , ( "attachment", Just "AB2C3D4E" )
                                , ( "attachment", Just "AB2C3D4E" )
                                , ( "annotation", Just "HI9J2K3L" )
                                , ( "note", Just "BC3D4E5F" )
                                ]
                            )
            , test "deleted is read whether the API sends 1 or true, and defaults to false" <|
                \() ->
                    Result.map2 (++) (items Fixtures.itemsTop) (items Fixtures.itemsChildren)
                        |> Result.map (List.filter .deleted >> List.map .key)
                        |> Expect.equal (Ok [ "FG7H8I9J", "KL4M5N6P" ])
            , test "null md5/mtime survive verbatim in data (the 0.3 code crashed on them)" <|
                \() ->
                    items Fixtures.itemsChildren
                        |> Result.toMaybe
                        |> Maybe.andThen (byKey "IJ2K3L4M")
                        |> Maybe.map (\i -> ( dataField "md5" (Decode.null "null") i, dataField "mtime" (Decode.null "null") i ))
                        |> Expect.equal (Just ( Ok "null", Ok "null" ))
            , test "observedFields reports the non-schema annotation fields" <|
                \() ->
                    items Fixtures.itemsChildren
                        |> Result.toMaybe
                        |> Maybe.andThen (byKey "JK3L4M5N")
                        |> Maybe.map (Object.observedFields >> List.filter (String.startsWith "annotation"))
                        |> Expect.equal
                            (Just
                                [ "annotationColor"
                                , "annotationComment"
                                , "annotationPageLabel"
                                , "annotationPosition"
                                , "annotationSortIndex"
                                , "annotationText"
                                , "annotationType"
                                ]
                            )
            , test "library name comes from the envelope" <|
                \() ->
                    items Fixtures.itemsTop
                        |> Result.map (List.head >> Maybe.andThen .libraryName)
                        |> Expect.equal (Ok (Just "zot-sync tester"))
            , test "an object without key fails loudly; one without itemType does not" <|
                \() ->
                    ( Decode.decodeString Object.itemDecoder """{"version": 1, "data": {"itemType": "book"}}""" |> Result.toMaybe
                    , Decode.decodeString Object.itemDecoder """{"key": "AAAAAAAA", "version": 1, "data": {}}""" |> Result.map .itemType
                    )
                        |> Expect.equal ( Nothing, Ok "unknown" )
            , test "parentItem false or empty means top-level" <|
                \() ->
                    [ """{"key": "AAAAAAAA", "version": 1, "data": {"parentItem": false}}"""
                    , """{"key": "AAAAAAAA", "version": 1, "data": {"parentItem": ""}}"""
                    ]
                        |> List.map (Decode.decodeString Object.itemDecoder >> Result.map .parentKey)
                        |> Expect.equal [ Ok Nothing, Ok Nothing ]
            ]
        , describe "collections and searches"
            [ test "collections decode, including the trashed one" <|
                \() ->
                    Decode.decodeString (Decode.list Object.entityDecoder) Fixtures.collections
                        |> Result.map (List.map (\c -> ( c.key, c.version, c.deleted )))
                        |> Expect.equal (Ok [ ( "CQ2R3S4T", 110, False ), ( "DR3S4T5U", 111, False ), ( "ES4T5U6V", 112, True ) ])
            , test "searches have no meta and still decode" <|
                \() ->
                    Decode.decodeString (Decode.list Object.entityDecoder) Fixtures.searches
                        |> Result.map (List.map .key)
                        |> Expect.equal (Ok [ "SA2B3C4D" ])
            ]
        , test "settings decode to name/version/value, sorted by name" <|
            \() ->
                Decode.decodeString Object.settingsDecoder Fixtures.settings
                    |> Result.map (List.map (\s -> ( s.name, s.version )))
                    |> Expect.equal (Ok [ ( "lastPageIndex_u_HI9J2K3L", 115 ), ( "tagColors", 114 ) ])
        , test "deleted lists all object kinds" <|
            \() ->
                Decode.decodeString Object.deletedDecoder Fixtures.deleted
                    |> Expect.equal
                        (Ok
                            { collections = [ "ZZ2Y3X4W" ]
                            , searches = []
                            , items = [ "YY3X4W5V", "AB2C3D4E" ]
                            , tags = [ "obsolete" ]
                            , settings = [ "lastPageIndex_u_OLD23456" ]
                            }
                        )
        , test "deleted tolerates missing kinds" <|
            \() ->
                Decode.decodeString Object.deletedDecoder """{"items": ["AAAAAAAA"]}"""
                    |> Expect.equal (Ok { noDeletions | items = [ "AAAAAAAA" ] })
        , describe "key info"
            [ test "all groups" <|
                \() ->
                    Decode.decodeString Object.keyInfoDecoder Fixtures.keyInfoAllGroups
                        |> Expect.equal (Ok { userId = 1234567, username = Just "zot-sync-tester", userLibrary = True, groups = AllGroups })
            , test "listed groups only count when readable; no user library access" <|
                \() ->
                    Decode.decodeString Object.keyInfoDecoder Fixtures.keyInfoSomeGroups
                        |> Expect.equal (Ok { userId = 1234567, username = Just "zot-sync-tester", userLibrary = False, groups = SomeGroups [ 2345678 ] })
            ]
        , test "group metadata" <|
            \() ->
                Decode.decodeString Object.groupDecoder Fixtures.group
                    |> Result.map (\g -> ( g.id, g.version, g.name ))
                    |> Expect.equal (Ok ( 2345678, 7, "Sync Research Group" ))
        , test "versions" <|
            \() ->
                Decode.decodeString Object.versionsDecoder """{"AB2C3D4E": 120, "BC3D4E5F": 118}"""
                    |> Expect.equal (Ok (Dict.fromList [ ( "AB2C3D4E", 120 ), ( "BC3D4E5F", 118 ) ]))
        ]


noDeletions : Object.Deleted
noDeletions =
    Object.noDeletions
