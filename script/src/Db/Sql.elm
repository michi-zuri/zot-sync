module Db.Sql exposing
    ( Statement, statement, encodeStatement
    , identifier, qualified, literal
    )

{-| SQL as plain data. Values always travel as bind parameters; only DDL, which
Postgres cannot parameterise, embeds names and literals, and those go through
`identifier` and `literal`.

@docs Statement, statement, encodeStatement
@docs identifier, qualified, literal

-}

import Json.Encode as Encode


{-| -}
type alias Statement =
    { sql : String
    , params : List Encode.Value
    }


{-| -}
statement : String -> List Encode.Value -> Statement
statement sql params =
    { sql = sql, params = params }


{-| -}
encodeStatement : Statement -> Encode.Value
encodeStatement s =
    Encode.object
        [ ( "sql", Encode.string s.sql )
        , ( "params", Encode.list identity s.params )
        ]


{-| A quoted identifier: `a"b` becomes `"a""b"`. Zotero field names are
camelCase, so they must always be quoted anyway.
-}
identifier : String -> String
identifier name =
    "\"" ++ String.replace "\"" "\"\"" name ++ "\""


{-| -}
qualified : String -> String -> String
qualified schema name =
    identifier schema ++ "." ++ identifier name


{-| A string literal: `it's` becomes `'it''s'` (standard_conforming_strings).
-}
literal : String -> String
literal value =
    "'" ++ String.replace "'" "''" value ++ "'"
