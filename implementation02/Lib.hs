{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.String

{-
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
-}

-- Structure to Create composite type.
data (a :: k1) :<< (b :: k2)
infixr 5 :<<

-- Heterogeneous list
data HList (ts :: [Type]) where
    HNil  :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 &:
(&:) :: x -> HList xs -> HList (x ': xs)
(&:) = HCons

instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (HCons x xs) = show x ++ " &: " ++ show xs

type FieldName = Symbol
type StrLength = Nat

data TContent
    = TStrFixed
    | TStrVariable
    | TNested
    deriving (Eq, Show)

newtype FixedString (n :: StrLength) = FixedString String

instance IsString (FixedString n) where
    fromString = FixedString

data Content t where
    StrFixed    :: forall (fn::FieldName) (n::StrLength) . KnownNat n =>
        FixedString n -> Content ('TStrFixed :<< fn :<< n)
    StrVariable :: forall (fn::FieldName) .
        String -> Content ('TStrVariable :<< fn)
    Nested      :: forall (fn::FieldName) (ts :: [Type]) .
        HList ts -> Content ('TNested :<< fn :<< ts)

instance Show (Content t) where
    show _ = "content..."

{-
loadSchema :: FilePath -> IO PlainSchema
loadSchema path = read <$> readFile path
-}

