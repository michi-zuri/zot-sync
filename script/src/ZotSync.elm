module ZotSync exposing
    ( run, program
    , Command(..), FetchOptions, CheckOptions
    , Selection(..), selection, targetsFor, outcomeLine
    )

{-| zot-sync: mirror Zotero libraries into Postgres.

    zot - sync fetch --db postgres://... --key KEY             every library the key can read

    zot - sync fetch --db postgres://... --user ID [--key KEY] one user library

    zot - sync fetch --db postgres://... --group ID            one group library

    zot - sync fetch --db postgres://... --user ID --all       a user's library and groups

    zot - sync check --db postgres://... --key KEY             check the database and the key

The options and their precedence are zot-sync 0.3's, as long flags.

@docs run, program
@docs Command, FetchOptions, CheckOptions
@docs Selection, selection, targetsFor, outcomeLine

-}

import BackendTask exposing (BackendTask)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Sync
import Zotero.Api as Api
import Zotero.Library as Library exposing (Library(..))
import Zotero.Object exposing (GroupAccess(..))


{-| -}
type Command
    = Fetch FetchOptions
    | Check CheckOptions


{-| -}
type alias FetchOptions =
    { db : String
    , user : Maybe Int
    , group : Maybe Int
    , key : Maybe String
    , all : Bool
    , skip : Bool
    , skipFulltext : Bool
    , verbose : Bool
    , apiUrl : String
    }


{-| -}
type alias CheckOptions =
    { db : Maybe String
    , key : Maybe String
    , verbose : Bool
    , apiUrl : String
    }


{-| -}
run : Script
run =
    Script.withCliOptions program
        (\command ->
            case command of
                Fetch options ->
                    fetch options

                Check options ->
                    check options
        )


{-| -}
program : Program.Config Command
program =
    Program.config
        |> Program.add
            (OptionsParser.buildSubCommand "fetch" FetchOptions
                |> OptionsParser.with (Option.requiredKeywordArg "db" |> Option.withDescription "Postgres connection string, e.g. postgres://user:pass@host:5432/zotero")
                |> OptionsParser.with (Option.optionalKeywordArg "user" |> Option.validateMapIfPresent libraryId |> Option.withDescription "sync this user library")
                |> OptionsParser.with (Option.optionalKeywordArg "group" |> Option.validateMapIfPresent libraryId |> Option.withDescription "sync this group library")
                |> OptionsParser.with (Option.optionalKeywordArg "key" |> Option.withDescription "Zotero API key (https://www.zotero.org/settings/keys)")
                |> OptionsParser.with (Option.flag "all" |> Option.withDescription "also sync every group of the user (with --user or --key)")
                |> OptionsParser.with (Option.flag "skip" |> Option.withDescription "skip the user library (with --all or --key)")
                |> OptionsParser.with (Option.flag "skip-fulltext" |> Option.withDescription "do not sync the full-text content of attachments")
                |> OptionsParser.with (Option.flag "verbose" |> Option.withDescription "log every request")
                |> OptionsParser.with (Option.optionalKeywordArg "api-url" |> Option.withDefault Api.defaultBaseUrl |> Option.withDescription "Zotero API base URL")
                |> OptionsParser.map Fetch
                |> OptionsParser.withDescription "Sync Zotero libraries into the Postgres mirror."
            )
        |> Program.add
            (OptionsParser.buildSubCommand "check" CheckOptions
                |> OptionsParser.with (Option.optionalKeywordArg "db" |> Option.withDescription "check this database and create the zot-sync tables")
                |> OptionsParser.with (Option.optionalKeywordArg "key" |> Option.withDescription "show what this Zotero API key can read")
                |> OptionsParser.with (Option.flag "verbose" |> Option.withDescription "log every request")
                |> OptionsParser.with (Option.optionalKeywordArg "api-url" |> Option.withDefault Api.defaultBaseUrl |> Option.withDescription "Zotero API base URL")
                |> OptionsParser.map Check
                |> OptionsParser.withDescription "Check a database connection and what an API key can read."
            )


libraryId : String -> Result String Int
libraryId raw =
    case String.toInt raw of
        Just n ->
            if n > 0 && n <= 999999999 then
                Ok n

            else
                Err "must be a library id between 1 and 999999999"

        Nothing ->
            Err "must be a numeric library id"



-- WHICH LIBRARIES


{-| What the options ask for, with zot-sync 0.3's precedence:
`--all` (with `--user` or `--key`), then `--user`, then `--group`, then `--key`.
-}
type Selection
    = UserAndGroups { userId : Maybe Int, includeUser : Bool }
    | OneUser Int
    | OneGroup Int
    | KeyLibraries { includeUser : Bool }
    | NothingSelected


{-| -}
selection : FetchOptions -> Selection
selection options =
    if options.all && (options.user /= Nothing || options.key /= Nothing) then
        UserAndGroups { userId = options.user, includeUser = not options.skip }

    else
        case ( options.user, options.group, options.key ) of
            ( Just user, _, _ ) ->
                OneUser user

            ( Nothing, Just group, _ ) ->
                OneGroup group

            ( Nothing, Nothing, Just _ ) ->
                KeyLibraries { includeUser = not options.skip }

            ( Nothing, Nothing, Nothing ) ->
                NothingSelected


{-| The libraries a selection stands for; `--key` and `--all` ask the API.
-}
targetsFor : { api : Api.Config, verbose : Bool } -> Selection -> BackendTask Sync.Error (List Sync.Target)
targetsFor config chosen =
    let
        groupTarget : Int -> Sync.Target
        groupTarget id =
            { library = Group id, name = Nothing }
    in
    case chosen of
        OneUser id ->
            BackendTask.succeed [ { library = User id, name = Nothing } ]

        OneGroup id ->
            BackendTask.succeed [ groupTarget id ]

        NothingSelected ->
            BackendTask.succeed []

        UserAndGroups { userId, includeUser } ->
            (case userId of
                Just id ->
                    BackendTask.succeed ( id, Nothing )

                Nothing ->
                    Sync.keyInfo config |> BackendTask.map (\key -> ( key.info.userId, key.info.username ))
            )
                |> BackendTask.andThen
                    (\( id, username ) ->
                        Sync.userGroups config id
                            |> BackendTask.map
                                (\groups ->
                                    (if includeUser then
                                        [ { library = User id, name = username } ]

                                     else
                                        []
                                    )
                                        ++ List.map groupTarget groups
                                )
                    )

        KeyLibraries { includeUser } ->
            Sync.keyInfo config
                |> BackendTask.andThen
                    (\{ info } ->
                        let
                            user : List Sync.Target
                            user =
                                if includeUser && info.userLibrary then
                                    [ { library = User info.userId, name = info.username } ]

                                else
                                    []
                        in
                        case info.groups of
                            AllGroups ->
                                Sync.userGroups config info.userId
                                    |> BackendTask.map (\groups -> user ++ List.map groupTarget groups)

                            SomeGroups ids ->
                                BackendTask.succeed (user ++ List.map groupTarget ids)
                    )



-- COMMANDS


fetch : FetchOptions -> BackendTask FatalError ()
fetch options =
    let
        config : { api : Api.Config, verbose : Bool }
        config =
            { api = { baseUrl = options.apiUrl, apiKey = options.key }, verbose = options.verbose }
    in
    Sync.prepare options.db
        |> BackendTask.andThen (\_ -> targetsFor config (selection options))
        |> BackendTask.andThen
            (\targets ->
                if List.isEmpty targets then
                    Script.log "The database is fine. Now say what to sync: --user, --group or --key (see zot-sync fetch --help)."
                        |> BackendTask.map (\_ -> [])

                else
                    Sync.loadSchema { db = options.db, api = config.api, verbose = options.verbose }
                        |> BackendTask.andThen
                            (\loaded ->
                                (case loaded.warning of
                                    Just warning ->
                                        Script.log ("warning: " ++ warning)

                                    Nothing ->
                                        BackendTask.succeed ()
                                )
                                    |> BackendTask.andThen
                                        (\_ ->
                                            targets
                                                |> List.map
                                                    (syncOne
                                                        { db = options.db
                                                        , api = config.api
                                                        , verbose = options.verbose
                                                        , fulltext = not options.skipFulltext
                                                        , schema = loaded.schema
                                                        , schemaRevision = loaded.revision
                                                        }
                                                    )
                                                |> BackendTask.sequence
                                        )
                            )
            )
        |> BackendTask.mapError (Sync.errorToString >> FatalError.fromString)
        |> BackendTask.andThen
            (\results ->
                let
                    failed : Int
                    failed =
                        results |> List.filter (not << identity) |> List.length
                in
                if failed == 0 then
                    BackendTask.succeed ()

                else
                    BackendTask.fail
                        (FatalError.build
                            { title = "zot-sync"
                            , body = String.fromInt failed ++ " of " ++ String.fromInt (List.length results) ++ " libraries could not be synced (see above)."
                            }
                        )
            )


{-| Sync one library and report it. A failure is reported and the run moves
on to the next library; `False` marks it for the exit status.
-}
syncOne : Sync.Context -> Sync.Target -> BackendTask Sync.Error Bool
syncOne ctx target =
    Sync.syncLibrary ctx target
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\result ->
                Script.log (outcomeLine target result)
                    |> BackendTask.map
                        (\_ ->
                            case result of
                                Ok _ ->
                                    True

                                Err _ ->
                                    False
                        )
            )


{-| One line per library, e.g.

    zot_u_123 "My Library": version 0 -> 126 · 11 items, 3 collections · views updated
    zot_g_456 "Lab": up to date at version 99
    zot_g_789: FAILED: HTTP 403 from https://api.zotero.org/groups/789

-}
outcomeLine : Sync.Target -> Result Sync.Error Sync.Outcome -> String
outcomeLine target result =
    let
        label : String
        label =
            Library.schemaName target.library
    in
    case result of
        Ok outcome ->
            label
                ++ (outcome.name |> Maybe.map (\n -> " \"" ++ n ++ "\"") |> Maybe.withDefault "")
                ++ ": "
                ++ (if outcome.changed then
                        "version " ++ String.fromInt outcome.fromVersion ++ " -> " ++ String.fromInt outcome.toVersion ++ " · " ++ summary outcome.stats

                    else
                        "up to date at version "
                            ++ String.fromInt outcome.toVersion
                            ++ (case summaryParts outcome.stats of
                                    [] ->
                                        ""

                                    parts ->
                                        " · " ++ String.join ", " parts
                               )
                   )
                ++ (if outcome.viewsUpdated then
                        " · views updated"

                    else
                        ""
                   )

        Err error ->
            label ++ ": FAILED: " ++ Sync.errorToString error


summary : Sync.Stats -> String
summary stats =
    case summaryParts stats of
        [] ->
            "nothing new"

        parts ->
            String.join ", " parts


summaryParts : Sync.Stats -> List String
summaryParts stats =
    [ ( stats.items, "item", "items" )
    , ( stats.collections, "collection", "collections" )
    , ( stats.searches, "saved search", "saved searches" )
    , ( stats.settings, "setting", "settings" )
    , ( stats.fulltext, "full text", "full texts" )
    , ( stats.deleted, "deletion", "deletions" )
    ]
        |> List.filter (\( n, _, _ ) -> n > 0)
        |> List.map
            (\( n, one, many ) ->
                String.fromInt n
                    ++ " "
                    ++ (if n == 1 then
                            one

                        else
                            many
                       )
            )


check : CheckOptions -> BackendTask FatalError ()
check options =
    let
        config : { api : Api.Config, verbose : Bool }
        config =
            { api = { baseUrl = options.apiUrl, apiKey = options.key }, verbose = options.verbose }

        checkDb : BackendTask Sync.Error ()
        checkDb =
            case options.db of
                Just db ->
                    Sync.prepare db
                        |> BackendTask.andThen (\_ -> Script.log "Database OK: the zot-sync tables are in place.")

                Nothing ->
                    BackendTask.succeed ()

        checkKey : BackendTask Sync.Error ()
        checkKey =
            case options.key of
                Just _ ->
                    Sync.keyInfo config
                        |> BackendTask.andThen (\{ raw } -> Script.log (withoutKey raw))

                Nothing ->
                    BackendTask.succeed ()
    in
    if options.db == Nothing && options.key == Nothing then
        BackendTask.fail (FatalError.fromString "Nothing to check: pass --db, --key, or both.")

    else
        checkDb
            |> BackendTask.andThen (\_ -> checkKey)
            |> BackendTask.mapError (Sync.errorToString >> FatalError.fromString)


{-| Key info as zot-sync 0.3 printed it, minus the key itself.
-}
withoutKey : Decode.Value -> String
withoutKey raw =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) raw of
        Ok pairs ->
            pairs |> List.filter (\( k, _ ) -> k /= "key") |> Encode.object |> Encode.encode 4

        Err _ ->
            Encode.encode 4 raw
