{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- Schema definitions are known at compile time.

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Lib

-- TODO: use TH to generate something like this by loading schema from disk (../schemas)

type Schema1 = HList
   '[Content ('TStrFixed :<< ("A" :<< 3)),
     Content ('TStrFixed :<< ("B" :<< 4)),
     Content ('TStrVariable :<< "C")]

type Schema2 = HList
   '[Content ('TStrFixed :<< ("A" :<< 2)),
     Content ('TStrVariable :<< "B"),
     Content ('TNested :<< ("C" :<<
        '[Content ('TStrFixed :<< ("D" :<< 1)),
          Content ('TStrVariable :<< "E")]))]

main :: IO ()
main = do

    -- create samples
    let

        sample1 :: Schema1
        sample1
            = StrFixed @"A" @3 "abc"
           &: StrFixed @"B" @4 "abcd"
           &: StrVariable @"C" "test"
           &: HNil

        sample2 :: Schema2
        sample2
            = StrFixed @"A" @2 "ab"
           &: StrVariable @"B" "stringB"
           &: Nested @"C"
                ( StrFixed @"D" @1 "a"
               &: StrVariable @"E" "stringE"
               &: HNil
                )
           &: HNil

    print sample1
    print sample2

    -- parse sample

    -- manipulate sample

