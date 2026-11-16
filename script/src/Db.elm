module Db exposing (Error, errorToString, query, execute, transaction)

{-| Talks to Postgres through the `pgQuery` and `pgTransaction` functions in
`custom-backend-task.ts`. Everything here is a thin wrapper; the SQL itself is
built by the pure modules `Db.Layout`, `Db.Views` and `Db.Store`.

@docs Error, errorToString, query, execute, transaction

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Db.Sql as Sql exposing (Statement)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| -}
type alias Error =
    { message : String
    , code : Maybe String
    , detail : Maybe String
    , statement : Maybe String
    }


{-| -}
errorToString : Error -> String
errorToString error =
    [ Just error.message
    , error.detail
    , error.statement |> Maybe.map (\s -> "in: " ++ s)
    ]
        |> List.filterMap identity
        |> String.join "\n"


{-| Rows of a single statement.
-}
query : String -> Statement -> Decoder row -> BackendTask Error (List row)
query db statement rowDecoder =
    call "pgQuery"
        (Encode.object
            [ ( "db", Encode.string db )
            , ( "sql", Encode.string statement.sql )
            , ( "params", Encode.list identity statement.params )
            ]
        )
        (Decode.field "rows" (Decode.list rowDecoder))


{-| A single statement; returns the number of affected rows.
-}
execute : String -> Statement -> BackendTask Error Int
execute db statement =
    call "pgQuery"
        (Encode.object
            [ ( "db", Encode.string db )
            , ( "sql", Encode.string statement.sql )
            , ( "params", Encode.list identity statement.params )
            ]
        )
        (Decode.field "rowCount" Decode.int)


{-| Statements in one transaction: all of them commit, or none.
-}
transaction : String -> List Statement -> BackendTask Error ()
transaction db statements =
    if List.isEmpty statements then
        BackendTask.succeed ()

    else
        call "pgTransaction"
            (Encode.object
                [ ( "db", Encode.string db )
                , ( "statements", Encode.list Sql.encodeStatement statements )
                ]
            )
            (Decode.succeed ())


call : String -> Encode.Value -> Decoder a -> BackendTask Error a
call portName input decoder =
    BackendTask.Custom.run portName input (outcomeDecoder decoder)
        |> BackendTask.quiet
        |> BackendTask.mapError (\{ recoverable } -> bridgeError portName recoverable)
        |> BackendTask.andThen BackendTask.fromResult


outcomeDecoder : Decoder a -> Decoder (Result Error a)
outcomeDecoder decoder =
    Decode.field "ok" Decode.bool
        |> Decode.andThen
            (\ok ->
                if ok then
                    Decode.map Ok decoder

                else
                    Decode.map Err (Decode.field "error" errorDecoder)
            )


errorDecoder : Decoder Error
errorDecoder =
    Decode.map4 Error
        (Decode.oneOf [ Decode.field "message" Decode.string, Decode.succeed "unknown database error" ])
        (Decode.maybe (Decode.field "code" Decode.string))
        (Decode.maybe (Decode.field "detail" Decode.string))
        (Decode.maybe (Decode.field "statement" Decode.string))


bridgeError : String -> BackendTask.Custom.Error -> Error
bridgeError portName error =
    let
        message : String
        message =
            case error of
                BackendTask.Custom.CustomBackendTaskException value ->
                    Decode.decodeValue (Decode.oneOf [ Decode.string, Decode.field "message" Decode.string ]) value
                        |> Result.withDefault (Encode.encode 0 value)

                BackendTask.Custom.NonJsonException text ->
                    text

                BackendTask.Custom.DecodeError decodeError ->
                    "unexpected response: " ++ Decode.errorToString decodeError

                BackendTask.Custom.CustomBackendTaskNotDefined { name } ->
                    "custom-backend-task.ts does not export " ++ name

                BackendTask.Custom.MissingCustomBackendTaskFile ->
                    "custom-backend-task.ts is missing"

                BackendTask.Custom.ErrorInCustomBackendTaskFile ->
                    "custom-backend-task.ts failed to load"

                BackendTask.Custom.ExportIsNotFunction ->
                    "custom-backend-task.ts export is not a function"

                BackendTask.Custom.Error ->
                    "unknown error"
    in
    { message = "database bridge (" ++ portName ++ "): " ++ message, code = Nothing, detail = Nothing, statement = Nothing }
