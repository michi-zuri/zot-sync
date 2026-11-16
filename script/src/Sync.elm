module Sync exposing
    ( Context, Target, Outcome, Stats, Error(..), errorToString
    , prepare, LoadedSchema, loadSchema, syncLibrary
    , keyInfo, userGroups
    , restartLimit, viewsGeneration, fulltextParallelism
    )

{-| One-way sync of Zotero libraries into the Postgres mirror, following the
official Zotero client (chrome/content/zotero/xpcom/sync/syncEngine.js):

1.  `settings?since=V` with `If-Modified-Since-Version: V`. A 304 means nothing
    changed, and the run stops there. Otherwise its `Last-Modified-Version` is
    the version this pass syncs to.
2.  For collections, searches and items: `?format=versions&since=V` lists what
    changed, then the stale objects are fetched by key, 100 per request, and
    stored. Items include child notes, attachments and annotations, and the
    trash.
3.  `deleted?since=V` removes what was deleted. A 409 means V is older than
    the server's delete log; the pass then runs as a full reconciliation,
    listing every key and dropping local objects the server no longer has.
4.  Full-text content of attachments (`fulltext?since=V`).

Every listing must report the same library version as step 1; if the library
changed mid-pass, the pass waits and starts over (up to `restartLimit`). The
library's version is only recorded after a pass succeeds, together with the
regenerated views, so an interrupted run is simply repeated next time; all
writes are idempotent.

@docs Context, Target, Outcome, Stats, Error, errorToString
@docs prepare, LoadedSchema, loadSchema, syncLibrary
@docs keyInfo, userGroups
@docs restartLimit, viewsGeneration, fulltextParallelism

-}

import BackendTask exposing (BackendTask)
import Db
import Db.Layout as Layout
import Db.Sql exposing (Statement)
import Db.Store as Store exposing (Table(..))
import Db.Views as Views
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages.Script as Script
import Zotero.Api as Api exposing (Kind(..))
import Zotero.Http as Http
import Zotero.Library as Library exposing (Library(..))
import Zotero.Object as Object exposing (Item)
import Zotero.Schema as Schema exposing (Schema)


{-| -}
type alias Context =
    { db : String
    , api : Api.Config
    , verbose : Bool
    , fulltext : Bool
    , schema : Schema
    , schemaRevision : String
    }


{-| A library to sync, with its name if it is already known (key info).
-}
type alias Target =
    { library : Library
    , name : Maybe String
    }


{-| -}
type alias Stats =
    { items : Int
    , collections : Int
    , searches : Int
    , settings : Int
    , fulltext : Int
    , deleted : Int
    }


{-| -}
type alias Outcome =
    { library : Library
    , name : Maybe String
    , fromVersion : Int
    , toVersion : Int
    , changed : Bool
    , viewsUpdated : Bool
    , stats : Stats
    }


{-| `Restart` and `DeleteLogGone` steer a pass and never leave this module.
-}
type Error
    = HttpError Http.Error
    | DbError Db.Error
    | Unexpected { url : String, status : Int, body : String }
    | BadResponse { url : String, problem : String }
    | MissingVersion String
    | KeepsChanging Int
    | LegacyLayout (List String)
    | KeyRejected
    | Restart
    | DeleteLogGone


{-| -}
errorToString : Error -> String
errorToString error =
    case error of
        HttpError e ->
            Http.errorToString e

        DbError e ->
            "database error: " ++ Db.errorToString e

        Unexpected { url, status, body } ->
            "HTTP "
                ++ String.fromInt status
                ++ " from "
                ++ url
                ++ (if String.isEmpty (String.trim body) then
                        ""

                    else
                        ": " ++ String.left 200 (String.trim body)
                   )

        BadResponse { url, problem } ->
            "unexpected response from " ++ url ++ ": " ++ problem

        MissingVersion url ->
            "no Last-Modified-Version header from " ++ url

        KeepsChanging attempts ->
            "the library kept changing during " ++ String.fromInt attempts ++ " attempts; try again later"

        LegacyLayout tables ->
            Layout.legacyMessage tables

        KeyRejected ->
            "Zotero rejected the API key (HTTP 403); check it at https://www.zotero.org/settings/keys"

        Restart ->
            "the library changed during the sync"

        DeleteLogGone ->
            "the server's delete log no longer reaches back to the last sync"


{-| -}
restartLimit : Int
restartLimit =
    5


{-| Bump when the view generator changes, so every library's views are
regenerated once by the new release.
-}
viewsGeneration : Int
viewsGeneration =
    1


{-| Full-text content is fetched per item; this many requests at a time.
-}
fulltextParallelism : Int
fulltextParallelism =
    4



-- RUN SETUP


{-| Refuse zot-sync 0.3 databases, then create or update the tables.
-}
prepare : String -> BackendTask Error ()
prepare db =
    Db.query db Layout.legacyCheck (Decode.field "name" Decode.string)
        |> BackendTask.mapError DbError
        |> BackendTask.andThen
            (\legacy ->
                if List.isEmpty legacy then
                    Db.transaction db Layout.setup
                        |> BackendTask.mapError DbError
                        |> BackendTask.map (\_ -> ())

                else
                    BackendTask.fail (LegacyLayout legacy)
            )


{-| -}
type alias LoadedSchema =
    { schema : Schema
    , revision : String
    , warning : Maybe String
    }


{-| The Zotero schema, revalidated with `If-None-Match` against the copy in
`zotero.schema`. If Zotero cannot be reached, the cached copy is used.
-}
loadSchema : { db : String, api : Api.Config, verbose : Bool } -> BackendTask Error LoadedSchema
loadSchema config =
    Db.query config.db Store.cachedSchema Store.cachedSchemaDecoder
        |> BackendTask.mapError DbError
        |> BackendTask.andThen
            (\rows ->
                let
                    cached : Maybe Store.CachedSchema
                    cached =
                        List.head rows

                    url : String
                    url =
                        Api.schemaUrl config.api

                    conditional : List ( String, String )
                    conditional =
                        case cached |> Maybe.andThen .etag of
                            Just etag ->
                                [ ( "If-None-Match", etag ) ]

                            Nothing ->
                                []
                in
                Http.get config.verbose url (Api.headers config.api ++ conditional)
                    |> BackendTask.toResult
                    |> BackendTask.andThen
                        (\result ->
                            case result of
                                Ok response ->
                                    if response.status == 200 then
                                        case Decode.decodeValue Schema.decoder response.body of
                                            Ok schema ->
                                                let
                                                    fresh : Store.CachedSchema
                                                    fresh =
                                                        { version = schema.version, etag = Http.header "etag" response, document = response.body }
                                                in
                                                Db.execute config.db (Store.storeSchema fresh)
                                                    |> BackendTask.mapError DbError
                                                    |> BackendTask.map (\_ -> loaded schema fresh Nothing)

                                            Err problem ->
                                                fallback cached "it did not decode" (BadResponse { url = url, problem = Decode.errorToString problem })

                                    else if response.status == 304 then
                                        fallback cached "" (unexpected response)

                                    else
                                        fallback cached ("HTTP " ++ String.fromInt response.status) (unexpected response)

                                Err httpError ->
                                    fallback cached (Http.errorToString httpError) (HttpError httpError)
                        )
            )


fallback : Maybe Store.CachedSchema -> String -> Error -> BackendTask Error LoadedSchema
fallback cached reason error =
    case cached of
        Just copy ->
            case Decode.decodeValue Schema.decoder copy.document of
                Ok schema ->
                    if String.isEmpty reason then
                        BackendTask.succeed (loaded schema copy Nothing)

                    else
                        BackendTask.succeed
                            (loaded schema copy (Just ("could not refresh the Zotero schema (" ++ reason ++ "); using the cached version " ++ String.fromInt copy.version)))

                Err problem ->
                    BackendTask.fail (BadResponse { url = "zotero.schema", problem = Decode.errorToString problem })

        Nothing ->
            BackendTask.fail error


loaded : Schema -> Store.CachedSchema -> Maybe String -> LoadedSchema
loaded schema copy warning =
    { schema = schema
    , revision = "views-" ++ String.fromInt viewsGeneration ++ ":" ++ Maybe.withDefault ("v" ++ String.fromInt copy.version) copy.etag
    , warning = warning
    }



-- DISCOVERY


{-| What an API key can read.
-}
keyInfo : { c | api : Api.Config, verbose : Bool } -> BackendTask Error { info : Object.KeyInfo, raw : Decode.Value }
keyInfo config =
    get config (Api.keyInfoUrl config.api) []
        |> BackendTask.andThen
            (\response ->
                if response.status == 403 then
                    BackendTask.fail KeyRejected

                else
                    expectStatus [ 200 ] response
            )
        |> BackendTask.andThen
            (\response ->
                decodeBody Object.keyInfoDecoder response
                    |> BackendTask.map (\info -> { info = info, raw = response.body })
            )


{-| Ids of the groups a user belongs to (public ones, or all with a key that
can read them).
-}
userGroups : { c | api : Api.Config, verbose : Bool } -> Int -> BackendTask Error (List Int)
userGroups config userId =
    get config (Api.userGroupsUrl config.api userId) []
        |> BackendTask.andThen (expectStatus [ 200 ])
        |> BackendTask.andThen (decodeBody Object.versionsDecoder)
        |> BackendTask.map (Dict.keys >> List.filterMap String.toInt >> List.sort)



-- ONE LIBRARY


{-| -}
syncLibrary : Context -> Target -> BackendTask Error Outcome
syncLibrary ctx target =
    let
        library : Library
        library =
            target.library
    in
    dbExec ctx (Store.ensureLibrary library)
        |> BackendTask.andThen (\_ -> dbQuery ctx (Store.libraryState library) Store.libraryStateDecoder)
        |> BackendTask.map (List.head >> Maybe.withDefault { version = 0, viewsRevision = Nothing, name = Nothing, fulltextVersion = 0 })
        |> BackendTask.andThen
            (\state ->
                dbQuery ctx (Store.startRun library state.version) Store.runIdDecoder
                    |> BackendTask.map (List.head >> Maybe.withDefault 0)
                    |> BackendTask.andThen
                        (\runId ->
                            syncTracked ctx target state runId
                                |> BackendTask.onError
                                    (\error ->
                                        -- best effort: the run is failed either way
                                        Db.execute ctx.db (Store.failRun runId (errorToString error))
                                            |> BackendTask.toResult
                                            |> BackendTask.andThen (\_ -> BackendTask.fail error)
                                    )
                        )
            )


syncTracked : Context -> Target -> Store.LibraryState -> Int -> BackendTask Error Outcome
syncTracked ctx target state runId =
    let
        library : Library
        library =
            target.library
    in
    groupMetadata ctx library
        |> BackendTask.andThen
            (\group ->
                pass ctx library (initialMode state.version) 1
                    |> BackendTask.andThen
                        (\result ->
                            let
                                ( changed, passed ) =
                                    case result of
                                        Nothing ->
                                            ( False, { version = state.version, stats = noStats, libraryName = Nothing, full = False } )

                                        Just p ->
                                            ( True, p )
                            in
                            catchUpFulltext ctx library state passed
                                |> BackendTask.andThen (finish ctx target state runId group changed)
                        )
            )


finish : Context -> Target -> Store.LibraryState -> Int -> Maybe Object.Group -> Bool -> Progress -> BackendTask Error Outcome
finish ctx target state runId group changed progress =
    let
        library : Library
        library =
            target.library

        -- Fresh information first, then what was stored, so the
        -- name is stable from run to run.
        name : Maybe String
        name =
            case group of
                Just g ->
                    Just g.name

                Nothing ->
                    progress.libraryName
                        |> orElse state.name
                        |> orElse target.name

        viewsNeeded : Bool
        viewsNeeded =
            changed || state.viewsRevision /= Just ctx.schemaRevision
    in
    (if viewsNeeded then
        planViews ctx library

     else
        BackendTask.succeed []
    )
        |> BackendTask.andThen
            (\viewStatements ->
                dbTx ctx
                    (viewStatements
                        ++ [ Store.finishLibrary library
                                { version = progress.version
                                , fulltextVersion =
                                    if ctx.fulltext then
                                        Just progress.version

                                    else
                                        Nothing
                                , name = name
                                , groupData = Maybe.map .data group
                                , viewsRevision =
                                    if viewsNeeded then
                                        Just ctx.schemaRevision

                                    else
                                        Nothing
                                }
                           , Store.finishRun runId progress.version (encodeStats progress.stats)
                           ]
                    )
            )
        |> BackendTask.map
            (\_ ->
                { library = library
                , name = name
                , fromVersion = state.version
                , toVersion = progress.version
                , changed = changed
                , viewsUpdated = viewsNeeded
                , stats = progress.stats
                }
            )


{-| Full text has its own version, so a run with --skip-fulltext never makes a
later run miss older full text: each run catches up from where full text was
last synced, even when the rest of the library is unchanged.
-}
catchUpFulltext : Context -> Library -> Store.LibraryState -> Progress -> BackendTask Error Progress
catchUpFulltext ctx library state progress =
    if ctx.fulltext && (state.fulltextVersion < progress.version || progress.full) then
        syncFulltext ctx
            library
            (if progress.full || state.fulltextVersion == 0 then
                Full

             else
                Since state.fulltextVersion
            )
            progress

    else
        BackendTask.succeed progress


groupMetadata : Context -> Library -> BackendTask Error (Maybe Object.Group)
groupMetadata ctx library =
    case library of
        Group groupId ->
            get ctx (Api.groupUrl ctx.api groupId) []
                |> BackendTask.andThen (expectStatus [ 200 ])
                |> BackendTask.andThen (decodeBody Object.groupDecoder)
                |> BackendTask.map Just

        User _ ->
            BackendTask.succeed Nothing


planViews : Context -> Library -> BackendTask Error (List Statement)
planViews ctx library =
    BackendTask.map2
        (\existing observed ->
            Views.plan
                { library = library
                , schema = ctx.schema
                , observed =
                    observed
                        |> List.foldr (\( itemType, field ) acc -> Dict.update itemType (\fs -> Just (field :: Maybe.withDefault [] fs)) acc) Dict.empty
                , existing = Views.groupExisting existing
                }
        )
        (dbQuery ctx (Views.existingColumnsQuery library) Views.existingColumnsDecoder)
        (dbQuery ctx (Views.observedFieldsQuery library) Views.observedFieldsDecoder)



-- A PASS


type Mode
    = Full
    | Since Int


initialMode : Int -> Mode
initialMode version =
    if version > 0 then
        Since version

    else
        Full


sinceOf : Mode -> Int
sinceOf mode =
    case mode of
        Full ->
            0

        Since version ->
            version


type alias Progress =
    { version : Int
    , stats : Stats
    , libraryName : Maybe String
    , full : Bool
    }


noStats : Stats
noStats =
    { items = 0, collections = 0, searches = 0, settings = 0, fulltext = 0, deleted = 0 }


{-| `Nothing` when the library has not changed since the last sync.
-}
pass : Context -> Library -> Mode -> Int -> BackendTask Error (Maybe Progress)
pass ctx library mode attempt =
    passOnce ctx library mode
        |> BackendTask.onError
            (\error ->
                case error of
                    Restart ->
                        if attempt >= restartLimit then
                            BackendTask.fail (KeepsChanging attempt)

                        else
                            note (Library.describe library ++ " changed while syncing; starting over")
                                |> BackendTask.andThen (\_ -> Script.sleep (1000 * 2 ^ (attempt - 1)))
                                |> BackendTask.andThen (\_ -> pass ctx library mode (attempt + 1))

                    DeleteLogGone ->
                        note ("The delete log of " ++ Library.describe library ++ " no longer reaches the last sync; reconciling every object")
                            |> BackendTask.andThen (\_ -> pass ctx library Full attempt)

                    _ ->
                        BackendTask.fail error
            )


passOnce : Context -> Library -> Mode -> BackendTask Error (Maybe Progress)
passOnce ctx library mode =
    fetchSettings ctx library mode
        |> BackendTask.andThen
            (\fetched ->
                case fetched of
                    Nothing ->
                        BackendTask.succeed Nothing

                    Just ( version, settings ) ->
                        { version = version, stats = noStats, libraryName = Nothing, full = mode == Full }
                            |> writeSettings ctx library mode settings
                            |> BackendTask.andThen (syncKind ctx library mode Collections)
                            |> BackendTask.andThen (syncKind ctx library mode Searches)
                            |> BackendTask.andThen (syncKind ctx library mode Items)
                            |> BackendTask.andThen (syncDeletions ctx library mode)
                            |> BackendTask.map Just
            )


fetchSettings : Context -> Library -> Mode -> BackendTask Error (Maybe ( Int, List Object.Setting ))
fetchSettings ctx library mode =
    let
        since : Int
        since =
            sinceOf mode

        conditional : List ( String, String )
        conditional =
            if since > 0 then
                [ ( "If-Modified-Since-Version", String.fromInt since ) ]

            else
                []
    in
    get ctx (Api.settingsUrl ctx.api library since) conditional
        |> BackendTask.andThen
            (\response ->
                if response.status == 304 then
                    BackendTask.succeed Nothing

                else
                    expectStatus [ 200 ] response
                        |> BackendTask.andThen (\_ -> requireVersion response)
                        |> BackendTask.andThen
                            (\version ->
                                decodeBody Object.settingsDecoder response
                                    |> BackendTask.map (\settings -> Just ( version, settings ))
                            )
            )


writeSettings : Context -> Library -> Mode -> List Object.Setting -> Progress -> BackendTask Error Progress
writeSettings ctx library mode settings progress =
    dbTx ctx
        ((if List.isEmpty settings then
            []

          else
            [ Store.upsertSettings library settings ]
         )
            ++ (case mode of
                    Full ->
                        [ Store.deleteKeysExcept SettingTable library (List.map .name settings) ]

                    Since _ ->
                        []
               )
        )
        |> BackendTask.map (\_ -> { progress | stats = addStat SettingsStat (List.length settings) progress.stats })


syncKind : Context -> Library -> Mode -> Kind -> Progress -> BackendTask Error Progress
syncKind ctx library mode kind progress =
    let
        table : Table
        table =
            tableOf kind
    in
    listVersions ctx (Api.versionsUrl ctx.api library kind (sinceOf mode)) progress.version
        |> BackendTask.andThen
            (\remote ->
                localVersionsFor ctx table library mode (Dict.keys remote)
                    |> BackendTask.andThen
                        (\local ->
                            chunks Api.batchSize (stale remote local)
                                |> List.foldl (\batch soFar -> soFar |> BackendTask.andThen (fetchBatch ctx library kind batch))
                                    (BackendTask.succeed progress)
                                |> BackendTask.andThen
                                    (\after ->
                                        case mode of
                                            Full ->
                                                dbExec ctx (Store.deleteKeysExcept table library (Dict.keys remote))
                                                    |> BackendTask.map (\n -> { after | stats = addStat DeletedStat n after.stats })

                                            Since _ ->
                                                BackendTask.succeed after
                                    )
                        )
            )


fetchBatch : Context -> Library -> Kind -> List String -> Progress -> BackendTask Error Progress
fetchBatch ctx library kind keys progress =
    get ctx (Api.objectsUrl ctx.api library kind keys) []
        |> BackendTask.andThen (expectStatus [ 200 ])
        |> BackendTask.andThen
            (\response ->
                case kind of
                    Items ->
                        decodeBody (Decode.list Object.itemDecoder) response
                            |> BackendTask.andThen
                                (\items ->
                                    dbTx ctx
                                        (if List.isEmpty items then
                                            []

                                         else
                                            [ Store.upsertItems library items, Store.recordItemFields library (fieldPairs items) ]
                                        )
                                        |> BackendTask.map
                                            (\_ ->
                                                { progress
                                                    | stats = addStat ItemsStat (List.length items) progress.stats
                                                    , libraryName = firstJust progress.libraryName (List.head items |> Maybe.andThen .libraryName)
                                                }
                                            )
                                )

                    _ ->
                        decodeBody (Decode.list Object.entityDecoder) response
                            |> BackendTask.andThen
                                (\entities ->
                                    dbTx ctx
                                        (if List.isEmpty entities then
                                            []

                                         else
                                            [ Store.upsertEntities (tableOf kind) library entities ]
                                        )
                                        |> BackendTask.map (\_ -> { progress | stats = addStat (kindStat kind) (List.length entities) progress.stats })
                                )
            )


syncDeletions : Context -> Library -> Mode -> Progress -> BackendTask Error Progress
syncDeletions ctx library mode progress =
    case mode of
        Full ->
            -- a full pass already dropped everything the server did not list
            BackendTask.succeed progress

        Since since ->
            get ctx (Api.deletedUrl ctx.api library since) []
                |> BackendTask.andThen
                    (\response ->
                        if response.status == 409 then
                            BackendTask.fail DeleteLogGone

                        else
                            expectStatus [ 200 ] response
                                |> BackendTask.andThen (atVersion progress.version)
                                |> BackendTask.andThen (decodeBody Object.deletedDecoder)
                    )
                |> BackendTask.andThen
                    (\deleted ->
                        let
                            targets : List ( Table, List String )
                            targets =
                                [ ( ItemTable, deleted.items )
                                , ( FulltextTable, deleted.items )
                                , ( CollectionTable, deleted.collections )
                                , ( SearchTable, deleted.searches )
                                , ( SettingTable, deleted.settings )
                                ]
                                    |> List.filter (Tuple.second >> List.isEmpty >> not)
                        in
                        dbTx ctx (List.map (\( table, keys ) -> Store.deleteKeys table library keys) targets)
                            |> BackendTask.map
                                (\counts ->
                                    List.map2 Tuple.pair (List.map Tuple.first targets) counts
                                        |> List.filter (\( table, _ ) -> table /= FulltextTable)
                                        |> List.map Tuple.second
                                        |> List.sum
                                        |> (\n -> { progress | stats = addStat DeletedStat n progress.stats })
                                )
                    )


type Fulltext
    = Found { key : String, version : Int, data : Decode.Value }
    | Gone String


syncFulltext : Context -> Library -> Mode -> Progress -> BackendTask Error Progress
syncFulltext ctx library mode progress =
    if not ctx.fulltext then
        BackendTask.succeed progress

    else
        -- Full-text rows carry their own versions, so a library change during
        -- this step needs no restart: anything newer is listed again next run.
        get ctx (Api.fulltextVersionsUrl ctx.api library (sinceOf mode)) []
            |> BackendTask.andThen (expectStatus [ 200 ])
            |> BackendTask.andThen (decodeBody Object.versionsDecoder)
            |> BackendTask.andThen
                (\remote ->
                    localVersionsFor ctx FulltextTable library mode (Dict.keys remote)
                        |> BackendTask.andThen
                            (\local ->
                                stale remote local
                                    |> List.map (\key -> ( key, Dict.get key remote |> Maybe.withDefault 0 ))
                                    |> chunks fulltextParallelism
                                    |> List.foldl (\group soFar -> soFar |> BackendTask.andThen (fetchFulltext ctx library group))
                                        (BackendTask.succeed progress)
                                    |> BackendTask.andThen
                                        (\after ->
                                            case mode of
                                                Full ->
                                                    dbExec ctx (Store.deleteKeysExcept FulltextTable library (Dict.keys remote))
                                                        |> BackendTask.map (\_ -> after)

                                                Since _ ->
                                                    BackendTask.succeed after
                                        )
                            )
                )


fetchFulltext : Context -> Library -> List ( String, Int ) -> Progress -> BackendTask Error Progress
fetchFulltext ctx library group progress =
    group
        |> List.map
            (\( key, version ) ->
                get ctx (Api.fulltextUrl ctx.api library key) []
                    |> BackendTask.andThen
                        (\response ->
                            if response.status == 200 then
                                BackendTask.succeed (Found { key = key, version = version, data = response.body })

                            else if response.status == 404 then
                                BackendTask.succeed (Gone key)

                            else
                                BackendTask.fail (unexpected response)
                        )
            )
        |> BackendTask.combine
        |> BackendTask.andThen
            (\results ->
                let
                    found =
                        List.filterMap
                            (\r ->
                                case r of
                                    Found f ->
                                        Just f

                                    Gone _ ->
                                        Nothing
                            )
                            results

                    gone =
                        List.filterMap
                            (\r ->
                                case r of
                                    Gone key ->
                                        Just key

                                    Found _ ->
                                        Nothing
                            )
                            results
                in
                dbTx ctx
                    ((if List.isEmpty found then
                        []

                      else
                        [ Store.upsertFulltext library found ]
                     )
                        ++ (if List.isEmpty gone then
                                []

                            else
                                [ Store.deleteKeys FulltextTable library gone ]
                           )
                    )
                    |> BackendTask.map (\_ -> { progress | stats = addStat FulltextStat (List.length found) progress.stats })
            )



-- HELPERS


listVersions : Context -> String -> Int -> BackendTask Error (Dict String Int)
listVersions ctx url expectedVersion =
    get ctx url []
        |> BackendTask.andThen (expectStatus [ 200 ])
        |> BackendTask.andThen (atVersion expectedVersion)
        |> BackendTask.andThen (decodeBody Object.versionsDecoder)


localVersionsFor : Context -> Table -> Library -> Mode -> List String -> BackendTask Error (Dict String Int)
localVersionsFor ctx table library mode keys =
    case mode of
        Full ->
            dbQuery ctx (Store.localVersions table library Nothing) Store.localVersionDecoder
                |> BackendTask.map Dict.fromList

        Since _ ->
            if List.isEmpty keys then
                BackendTask.succeed Dict.empty

            else
                dbQuery ctx (Store.localVersions table library (Just keys)) Store.localVersionDecoder
                    |> BackendTask.map Dict.fromList


{-| Remote keys that are missing locally or older there.
-}
stale : Dict String Int -> Dict String Int -> List String
stale remote local =
    remote
        |> Dict.toList
        |> List.filter
            (\( key, version ) ->
                case Dict.get key local of
                    Just localVersion ->
                        localVersion < version

                    Nothing ->
                        True
            )
        |> List.map Tuple.first


fieldPairs : List Item -> List ( String, String )
fieldPairs items =
    items |> List.concatMap (\item -> List.map (Tuple.pair item.itemType) (Object.observedFields item))


tableOf : Kind -> Table
tableOf kind =
    case kind of
        Collections ->
            CollectionTable

        Searches ->
            SearchTable

        Items ->
            ItemTable


chunks : Int -> List a -> List (List a)
chunks size list =
    case list of
        [] ->
            []

        _ ->
            List.take size list :: chunks size (List.drop size list)


{-| `first |> orElse fallback`
-}
orElse : Maybe a -> Maybe a -> Maybe a
orElse other first =
    firstJust first other


firstJust : Maybe a -> Maybe a -> Maybe a
firstJust first second =
    case first of
        Just _ ->
            first

        Nothing ->
            second


type Stat
    = ItemsStat
    | CollectionsStat
    | SearchesStat
    | SettingsStat
    | FulltextStat
    | DeletedStat


addStat : Stat -> Int -> Stats -> Stats
addStat stat n stats =
    case stat of
        ItemsStat ->
            { stats | items = stats.items + n }

        CollectionsStat ->
            { stats | collections = stats.collections + n }

        SearchesStat ->
            { stats | searches = stats.searches + n }

        SettingsStat ->
            { stats | settings = stats.settings + n }

        FulltextStat ->
            { stats | fulltext = stats.fulltext + n }

        DeletedStat ->
            { stats | deleted = stats.deleted + n }


kindStat : Kind -> Stat
kindStat kind =
    case kind of
        Collections ->
            CollectionsStat

        Searches ->
            SearchesStat

        Items ->
            ItemsStat


encodeStats : Stats -> Encode.Value
encodeStats stats =
    Encode.object
        [ ( "items", Encode.int stats.items )
        , ( "collections", Encode.int stats.collections )
        , ( "searches", Encode.int stats.searches )
        , ( "settings", Encode.int stats.settings )
        , ( "fulltext", Encode.int stats.fulltext )
        , ( "deleted", Encode.int stats.deleted )
        ]


get : { c | api : Api.Config, verbose : Bool } -> String -> List ( String, String ) -> BackendTask Error Http.Response
get ctx url extraHeaders =
    Http.get ctx.verbose url (Api.headers ctx.api ++ extraHeaders)
        |> BackendTask.mapError HttpError


expectStatus : List Int -> Http.Response -> BackendTask Error Http.Response
expectStatus statuses response =
    if List.member response.status statuses then
        BackendTask.succeed response

    else
        BackendTask.fail (unexpected response)


unexpected : Http.Response -> Error
unexpected response =
    Unexpected
        { url = response.url
        , status = response.status
        , body =
            case Decode.decodeValue Decode.string response.body of
                Ok text ->
                    -- a body that is itself a JSON string reads better unwrapped
                    Decode.decodeString Decode.string text |> Result.withDefault text

                Err _ ->
                    Encode.encode 0 response.body
        }


requireVersion : Http.Response -> BackendTask Error Int
requireVersion response =
    case Http.lastModifiedVersion response of
        Just version ->
            BackendTask.succeed version

        Nothing ->
            BackendTask.fail (MissingVersion response.url)


{-| The listing must describe the library version this pass syncs to;
otherwise something changed mid-pass and the pass starts over.
-}
atVersion : Int -> Http.Response -> BackendTask Error Http.Response
atVersion expected response =
    requireVersion response
        |> BackendTask.andThen
            (\actual ->
                if actual == expected then
                    BackendTask.succeed response

                else
                    BackendTask.fail Restart
            )


decodeBody : Decoder a -> Http.Response -> BackendTask Error a
decodeBody decoder response =
    case Decode.decodeValue decoder response.body of
        Ok value ->
            BackendTask.succeed value

        Err problem ->
            BackendTask.fail (BadResponse { url = response.url, problem = Decode.errorToString problem })


dbQuery : Context -> Statement -> Decoder row -> BackendTask Error (List row)
dbQuery ctx statement decoder =
    Db.query ctx.db statement decoder |> BackendTask.mapError DbError


dbExec : Context -> Statement -> BackendTask Error Int
dbExec ctx statement =
    Db.execute ctx.db statement |> BackendTask.mapError DbError


dbTx : Context -> List Statement -> BackendTask Error (List Int)
dbTx ctx statements =
    Db.transaction ctx.db statements |> BackendTask.mapError DbError


note : String -> BackendTask error ()
note message =
    Script.log message
