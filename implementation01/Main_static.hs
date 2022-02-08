{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib

-- not used at the moment
loadSchema :: FilePath -> IO PlainSchema
loadSchema path = read <$> readFile path

-- TODO: Schemas will be loaded from disk
-- and generated with template haskell at compile time.
-- For now just define some set of schemas manually.

sschema1 :: SSchema '[ '("A", 'StrFixed 3), '("B", 'StrFixed 4), '("C", 'StrVariable)]
sschema1
    = SField @"A" (SFixed @3)
   .- SField @"B" (SFixed @4)
   .- SField @"C" SVariable
   .- SNil

sschema2 :: SSchema '[ '("A", 'StrFixed 2), '("B", 'StrVariable), '("C", 'Nested '[ '("D", 'StrFixed 1), '("E", 'StrVariable)])]
sschema2
    = SField @"A" (SFixed @2)
   .- SField @"B" SVariable
   .- SField @"C"
        ( SNested
            ( SField @"D" (SFixed @1)
           .- SField @"E" SVariable
           .- SNil
            )
        )
   .- SNil

sschemas ::
    SSchemaDB
        '[ '(1, '[ '("A", 'StrFixed 3), '("B", 'StrFixed 4), '("C", 'StrVariable)])
         , '(2, '[ '("A", 'StrFixed 2), '("B", 'StrVariable), '("C", 'Nested '[ '("D", 'StrFixed 1), '("E", 'StrVariable)])])
         ]
sschemas =
    SIndexed @1 sschema1
        .= SIndexed @2 sschema2
        .= SDBNil

exampleChunk1 :: String
exampleChunk1 = "1aaabbbb3ccc"

exampleChunk2 :: String
exampleChunk2 = "1.......0"

exampleChunk3 :: String
exampleChunk3 = "2aa1bd2ee"

exampleChunk4 :: String
exampleChunk4 = "2..0.0"

main :: IO ()
main = do
    let schema = SIndexed @2 sschema2

    -- create sample
    do
        let x = TIndexed
                    (TCons (TFixed "aa") (TCons (TVariable "b")
                    (TCons (TNested (TCons (TFixed "d") (TCons
                    (TVariable "ee") TNil))) TNil)))
        print $ renderIndexed schema x

    -- parse and manipulate sample (accessor example)
    do
        let Right x = parseMsg (parseIndexed schema) "" exampleChunk3
            y = over (indexed . access @"C" . access @"E") ("ff" ++) x
        print x
        print y

        print $ renderIndexed schema x
        print (renderIndexed schema x == exampleChunk3)
        print $ renderIndexed schema y

