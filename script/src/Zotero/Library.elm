module Zotero.Library exposing
    ( Library(..)
    , id, schemaName, pathPrefix, typeName, zoteroId, describe
    )

{-| A Zotero library: someone's personal library or a group library.

@docs Library
@docs id, schemaName, pathPrefix, typeName, zoteroId, describe

-}


{-| -}
type Library
    = User Int
    | Group Int


{-| Key of the library in `zotero.library`: `u123` or `g456`.
-}
id : Library -> String
id library =
    case library of
        User n ->
            "u" ++ String.fromInt n

        Group n ->
            "g" ++ String.fromInt n


{-| The Postgres schema holding this library's query views. Same names as
zot-sync 0.3 (`zot_u_123`, `zot_g_456`), so existing queries keep working.
-}
schemaName : Library -> String
schemaName library =
    case library of
        User n ->
            "zot_u_" ++ String.fromInt n

        Group n ->
            "zot_g_" ++ String.fromInt n


{-| Path segments of the library in the Zotero Web API.
-}
pathPrefix : Library -> List String
pathPrefix library =
    case library of
        User n ->
            [ "users", String.fromInt n ]

        Group n ->
            [ "groups", String.fromInt n ]


{-| -}
typeName : Library -> String
typeName library =
    case library of
        User _ ->
            "user"

        Group _ ->
            "group"


{-| -}
zoteroId : Library -> Int
zoteroId library =
    case library of
        User n ->
            n

        Group n ->
            n


{-| Human-readable, for log lines.
-}
describe : Library -> String
describe library =
    typeName library ++ " library " ++ String.fromInt (zoteroId library)
