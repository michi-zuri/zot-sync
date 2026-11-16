module Zotero.Api exposing
    ( Config, defaultBaseUrl, headers
    , Kind(..), kindName
    , schemaUrl, keyInfoUrl, userGroupsUrl, groupUrl
    , settingsUrl, versionsUrl, objectsUrl, deletedUrl, fulltextVersionsUrl, fulltextUrl
    , batchSize
    )

{-| URLs and headers for the Zotero Web API v3, as the official Zotero client
builds them for syncing (chrome/content/zotero/xpcom/sync/syncAPIClient.js).

@docs Config, defaultBaseUrl, headers
@docs Kind, kindName
@docs schemaUrl, keyInfoUrl, userGroupsUrl, groupUrl
@docs settingsUrl, versionsUrl, objectsUrl, deletedUrl, fulltextVersionsUrl, fulltextUrl
@docs batchSize

-}

import Url.Builder as Url exposing (QueryParameter)
import Zotero.Library as Library exposing (Library)


{-| -}
type alias Config =
    { baseUrl : String
    , apiKey : Maybe String
    }


{-| -}
defaultBaseUrl : String
defaultBaseUrl =
    "https://api.zotero.org"


{-| The API key travels in a header, never in the URL, so it cannot leak into
logs.
-}
headers : Config -> List ( String, String )
headers config =
    ( "Zotero-API-Version", "3" )
        :: (case config.apiKey of
                Just key ->
                    [ ( "Zotero-API-Key", key ) ]

                Nothing ->
                    []
           )


{-| Object kinds that have versions and can be fetched by key.
-}
type Kind
    = Collections
    | Searches
    | Items


{-| -}
kindName : Kind -> String
kindName kind =
    case kind of
        Collections ->
            "collections"

        Searches ->
            "searches"

        Items ->
            "items"


keyParameter : Kind -> String
keyParameter kind =
    case kind of
        Collections ->
            "collectionKey"

        Searches ->
            "searchKey"

        Items ->
            "itemKey"


{-| The official client requests up to 100 objects by key. `limit` is set
explicitly because the API pages multi-object responses at 25 by default.
-}
batchSize : Int
batchSize =
    100


url : Config -> List String -> List QueryParameter -> String
url config path parameters =
    Url.crossOrigin (stripTrailingSlash config.baseUrl) path parameters


stripTrailingSlash : String -> String
stripTrailingSlash base =
    if String.endsWith "/" base then
        stripTrailingSlash (String.dropRight 1 base)

    else
        base


{-| `since=0` means "everything", which the API expresses by leaving it out.
-}
since : Int -> List QueryParameter
since version =
    if version > 0 then
        [ Url.int "since" version ]

    else
        []


trashed : Kind -> List QueryParameter
trashed kind =
    if kind == Items then
        [ Url.int "includeTrashed" 1 ]

    else
        []


{-| -}
schemaUrl : Config -> String
schemaUrl config =
    url config [ "schema" ] []


{-| -}
keyInfoUrl : Config -> String
keyInfoUrl config =
    url config [ "keys", "current" ] []


{-| Group ids and metadata versions of a user's groups.
-}
userGroupsUrl : Config -> Int -> String
userGroupsUrl config userId =
    url config [ "users", String.fromInt userId, "groups" ] [ Url.string "format" "versions" ]


{-| -}
groupUrl : Config -> Int -> String
groupUrl config groupId =
    url config [ "groups", String.fromInt groupId ] []


{-| -}
settingsUrl : Config -> Library -> Int -> String
settingsUrl config library version =
    url config (Library.pathPrefix library ++ [ "settings" ]) (since version)


{-| Keys and versions of everything of one kind changed after `version`.
-}
versionsUrl : Config -> Library -> Kind -> Int -> String
versionsUrl config library kind version =
    url config
        (Library.pathPrefix library ++ [ kindName kind ])
        (Url.string "format" "versions" :: trashed kind ++ since version)


{-| -}
objectsUrl : Config -> Library -> Kind -> List String -> String
objectsUrl config library kind keys =
    url config
        (Library.pathPrefix library ++ [ kindName kind ])
        (Url.string (keyParameter kind) (String.join "," keys) :: trashed kind ++ [ Url.int "limit" batchSize ])


{-| -}
deletedUrl : Config -> Library -> Int -> String
deletedUrl config library version =
    url config (Library.pathPrefix library ++ [ "deleted" ]) [ Url.int "since" version ]


{-| -}
fulltextVersionsUrl : Config -> Library -> Int -> String
fulltextVersionsUrl config library version =
    url config (Library.pathPrefix library ++ [ "fulltext" ]) (Url.string "format" "versions" :: since version)


{-| -}
fulltextUrl : Config -> Library -> String -> String
fulltextUrl config library itemKey =
    url config (Library.pathPrefix library ++ [ "items", itemKey, "fulltext" ]) []
