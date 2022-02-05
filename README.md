# Fictional data format(s)

Experimenting with data format processing.

It's a family of data formats, where each individual format is defined as
a 'schema'.

## Schemas

Each schema is in the form:

```haskell
type FieldName = String
type StrLength = Int
data Content
    = StrFixed StrLength
    | StrVariable
    | Nested [(FieldName, Content)]
type Schema = [(FieldName, Content)]
```

Examples:

```haskell
schema1 :: Schema
schema1 =
    [ ("A", StrFixed 3)
    , ("B", StrFixed 4)
    , ("C", StrVariable)
    ]

schema2 :: Schema
schema2 =
    [ ("A", StrFixed 2)
    , ("B", StrVariable)
    , ("C", Nested
        [ ("D", StrFixed 1)
        , ("E", StrVariable)
        ])
    ]
```

## Encoding

Data is encoded according to particular schema as follows:
- one character: schema index ['0'..'9']
- followed by some number of payload characters
- `StrFixed n` is encoded as a string (length must match)
- `StrVariable` is encoded as one char (length ['1'..'9']) + the string
  or '0' if the field is not present.
- `Nested` content is simply concatinated

Examples of valid data chunks (one chunk per line, but can be concatinated).

```
1aaabbbb3ccc    -- schema 1, ("aaa", "bbbb", Just "ccc")
1.......0       -- schema 1, ("...", "....", Nothing)
2aa1bd2ee       -- schema 2, ("aa", Just "b", ("d", Just "ee"))
2..0.0          -- schema 2, ("..", Nothing, (".", Nothing))
```

## Library API

Encoding and decoding API... to be defined...

### Dynamic usecase

Properties:
- Main application is compiled without any concrete schema.
- In the main app, schemas are loaded from disk at runtime.
- Operation to access a field (by name) can fail at runtime,
  if the field is not defined by the schema.

```haskell
-- lib
type Schemas = Map Int Schema

-- for test purposes, otherwise load from disk
loadSchemas :: IO Schemas
loadSchemas = return $ Map.fromList
        [ (1, schema1)
        , (2, schema2)
        ]

-- Structure to denote valid (Value or Subvalue)
data Value ? = ?

-- Structure to denote valid Message (that is: some concatinated Values)
data Message ? = ?

type Error = String

-- generate random sample
generateRandomSample :: Schema -> Seed -> (Value, Seed)

-- parse string, return Value and remaining String
parse :: Schema -> String -> Either Error (Value, String)

-- unparse value (Value and Schema must match)
build :: Schema -> Value -> String

-- field accessor (Lens, Prims... like)
-- to manipulate substructure (Value <-> Subvalue)
-- we want to call this at initialization stage, such that
-- it fails early if some field names are not defined.
accessor :: Schema -> FieldName -> Maybe (some optics)

-- create value from some (string like) content
mkValue :: Schema -> ? -> Value

-- app
main :: IO ()
main = do
    schemas <- loadSchemas

    -- get some concrete schema to work with, ignore errors for now
    let schema = Map.! schemas 2

        -- get required accessors, ignore errors for now
        (Just fieldA) = accessor schema "A"
        (Just fieldB) = accessor schema "B"
        (Just fieldC) = accessor schema "C"
        (Just fieldD) = ?   -- fieldC . (accessor to D within C)
        (Just fieldE) = ?   -- fieldC . (accessor to E within C)

        seed = ?
        sample = generateRandomSample schema seed
        str = build schema sample
        (Right (sample', str')) = parse schema str
            -- sample' == sample, str' == ""

        -- update sample, set field "A" to "aa", field "E" to ...
        sample2 = ? --

        -- create sample, out of some user defined data
        sample3 = mkValue schema ?

    -- parse and dump some input, handle any known schema
    x <- loadInputString
    -- parse 'x :: String' to a message
    -- fold over message, for each value, extract any present field
    -- or simply iterate in a procedural way
    -- for each chunk
        -- get schema (first character)
        -- if known schema, parse the rest of the chunk according to schema
        -- iterate over chunk's fields
        -- pretty print fields or dump as JSON
```

### Static, type safe usecase

Properties:
- Schemas are part of the application source tree.
- Schemas are loaded from disk at compile time (template haskell).
- Each schema becomes a *type*.
- For example: all schemas (together) are defined in a (TH generated)
  type family. To get a concrete schema, you apply a type family to a 'Nat',
  representing a schema index.
- Accessing non-existing field creates a compile time error.

```haskell
-- some template haskell magic
-- to be defined

main :: IO ()
main = do
    -- mkValue from primitive types
    -- build value to String (to wire format)
    -- parse string to value (from wire format)
    -- ... in a type safe way, that is:
    --  - compile error if non-existing field is accessed
```
