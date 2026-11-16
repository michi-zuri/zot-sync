module Zotero.Object exposing
    ( Item, itemDecoder
    , Entity, entityDecoder
    , Setting, settingsDecoder
    , Deleted, deletedDecoder, noDeletions
    , KeyInfo, GroupAccess(..), keyInfoDecoder
    , Group, groupDecoder
    , versionsDecoder
    , observedFields
    )

{-| Decoders for the objects the Zotero Web API returns.

Only the envelope is strict: an object needs a `key` and a `version`, because
that is the primary key of the mirror. Everything else is lenient, and the
object's `data` and `meta` are kept verbatim as JSON, so no individual field
can ever fail a sync.

@docs Item, itemDecoder
@docs Entity, entityDecoder
@docs Setting, settingsDecoder
@docs Deleted, deletedDecoder, noDeletions
@docs KeyInfo, GroupAccess, keyInfoDecoder
@docs Group, groupDecoder
@docs versionsDecoder
@docs observedFields

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| An item: a regular item, or a child note, attachment or annotation.
-}
type alias Item =
    { key : String
    , version : Int
    , itemType : String
    , parentKey : Maybe String
    , deleted : Bool
    , libraryName : Maybe String
    , data : Value
    , meta : Value
    }


{-| -}
itemDecoder : Decoder Item
itemDecoder =
    Decode.map8 Item
        keyDecoder
        versionDecoder
        (Decode.oneOf [ Decode.at [ "data", "itemType" ] Decode.string, Decode.succeed "unknown" ])
        (Decode.oneOf [ Decode.at [ "data", "parentItem" ] nonEmptyString, Decode.succeed Nothing ])
        (Decode.oneOf [ Decode.at [ "data", "deleted" ] lenientBool, Decode.succeed False ])
        (Decode.maybe (Decode.at [ "library", "name" ] Decode.string))
        (Decode.field "data" Decode.value)
        metaDecoder


{-| A collection or saved search.
-}
type alias Entity =
    { key : String
    , version : Int
    , deleted : Bool
    , data : Value
    , meta : Value
    }


{-| -}
entityDecoder : Decoder Entity
entityDecoder =
    Decode.map5 Entity
        keyDecoder
        versionDecoder
        (Decode.oneOf [ Decode.at [ "data", "deleted" ] lenientBool, Decode.succeed False ])
        (Decode.field "data" Decode.value)
        metaDecoder


{-| A synced setting such as `tagColors`.
-}
type alias Setting =
    { name : String
    , version : Int
    , value : Value
    }


{-| The `/settings` response: `{ "tagColors": { "value": ..., "version": 5 } }`.
-}
settingsDecoder : Decoder (List Setting)
settingsDecoder =
    Decode.keyValuePairs
        (Decode.map2 Tuple.pair
            (Decode.oneOf [ Decode.field "value" Decode.value, Decode.succeed Encode.null ])
            (Decode.oneOf [ Decode.field "version" Decode.int, Decode.succeed 0 ])
        )
        |> Decode.map
            (List.map (\( name, ( value, version ) ) -> { name = name, version = version, value = value })
                >> List.sortBy .name
            )


{-| Keys deleted since a version, from `/deleted`.
-}
type alias Deleted =
    { collections : List String
    , searches : List String
    , items : List String
    , tags : List String
    , settings : List String
    }


{-| -}
deletedDecoder : Decoder Deleted
deletedDecoder =
    Decode.map5 Deleted
        (stringList "collections")
        (stringList "searches")
        (stringList "items")
        (stringList "tags")
        (stringList "settings")


{-| -}
noDeletions : Deleted
noDeletions =
    { collections = [], searches = [], items = [], tags = [], settings = [] }


{-| What an API key can read, from `/keys/current`.
-}
type alias KeyInfo =
    { userId : Int
    , username : Maybe String
    , userLibrary : Bool
    , groups : GroupAccess
    }


{-| -}
type GroupAccess
    = AllGroups
    | SomeGroups (List Int)


{-| -}
keyInfoDecoder : Decoder KeyInfo
keyInfoDecoder =
    Decode.map4 KeyInfo
        (Decode.field "userID" Decode.int)
        (Decode.maybe (Decode.field "username" Decode.string))
        (Decode.oneOf [ Decode.at [ "access", "user", "library" ] lenientBool, Decode.succeed False ])
        (Decode.oneOf [ Decode.at [ "access", "groups" ] groupAccessDecoder, Decode.succeed (SomeGroups []) ])


groupAccessDecoder : Decoder GroupAccess
groupAccessDecoder =
    Decode.keyValuePairs (Decode.oneOf [ Decode.field "library" lenientBool, Decode.succeed True ])
        |> Decode.map
            (\entries ->
                let
                    readable : List String
                    readable =
                        entries |> List.filter Tuple.second |> List.map Tuple.first
                in
                if List.member "all" readable then
                    AllGroups

                else
                    readable |> List.filterMap String.toInt |> List.sort |> SomeGroups
            )


{-| Group metadata from `/groups/<id>`; `data` keeps the whole response.
-}
type alias Group =
    { id : Int
    , version : Int
    , name : String
    , data : Value
    }


{-| -}
groupDecoder : Decoder Group
groupDecoder =
    Decode.map4 Group
        (Decode.oneOf [ Decode.field "id" Decode.int, Decode.at [ "data", "id" ] Decode.int ])
        (Decode.oneOf [ Decode.field "version" Decode.int, Decode.at [ "data", "version" ] Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.at [ "data", "name" ] Decode.string, Decode.succeed "" ])
        Decode.value


{-| A `format=versions` response: `{ "ABCD2345": 12, ... }`.
-}
versionsDecoder : Decoder (Dict String Int)
versionsDecoder =
    Decode.dict Decode.int


{-| The field names present in an item's `data`. Notes, attachments and
annotations carry fields the schema does not list (`note`, `linkMode`,
`annotationText`, ...); recording what is actually seen lets the views pick
up new fields without a release of zot-sync.
-}
observedFields : Item -> List String
observedFields item =
    Decode.decodeValue (Decode.keyValuePairs Decode.value) item.data
        |> Result.map (List.map Tuple.first >> List.sort)
        |> Result.withDefault []



-- HELPERS


keyDecoder : Decoder String
keyDecoder =
    Decode.oneOf [ Decode.field "key" Decode.string, Decode.at [ "data", "key" ] Decode.string ]


versionDecoder : Decoder Int
versionDecoder =
    Decode.oneOf [ Decode.field "version" Decode.int, Decode.at [ "data", "version" ] Decode.int ]


metaDecoder : Decoder Value
metaDecoder =
    Decode.oneOf [ Decode.field "meta" Decode.value, Decode.succeed (Encode.object []) ]


stringList : String -> Decoder (List String)
stringList name =
    Decode.oneOf [ Decode.field name (Decode.list Decode.string), Decode.succeed [] ]


{-| `parentItem` is a key, or absent (`false` in some API versions).
-}
nonEmptyString : Decoder (Maybe String)
nonEmptyString =
    Decode.oneOf
        [ Decode.string
            |> Decode.map
                (\s ->
                    if String.isEmpty s then
                        Nothing

                    else
                        Just s
                )
        , Decode.succeed Nothing
        ]


{-| The API has sent `deleted` as `1` and as `true`; accept any spelling.
-}
lenientBool : Decoder Bool
lenientBool =
    Decode.oneOf
        [ Decode.bool
        , Decode.int |> Decode.map (\n -> n /= 0)
        , Decode.string |> Decode.map (\s -> List.member (String.toLower s) [ "1", "true", "yes" ])
        , Decode.null False
        ]
