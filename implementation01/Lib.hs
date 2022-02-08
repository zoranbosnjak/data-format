{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Monad
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import Data.Void
import GHC.TypeLits
import Text.Megaparsec
import Text.Megaparsec.Char

type FieldName = Symbol
type StrLength = Nat

type PlainFieldName = String
type PlainStrLength = Int

data RawContent fn len
  = StrFixed len
  | StrVariable
  | Nested (RawSchema fn len)
  deriving (Eq, Show, Read)

type RawSchema fn len = [(fn, RawContent fn len)]

-- type-level
type Content = RawContent FieldName StrLength
type Schema = RawSchema FieldName StrLength

-- term-level
type PlainContent = RawContent PlainFieldName PlainStrLength
type PlainSchema = RawSchema PlainFieldName PlainStrLength

type IndexedSchema = (Nat, Schema)

type Error = String

-- singleton of a schema
data SSchema (s :: Schema) where
  SNil :: SSchema '[]
  SCons :: SField fn c -> SSchema s -> SSchema ('(fn, c) : s)

deriving instance Show (SSchema s)

(.-) :: SField fn c -> SSchema s -> SSchema ('(fn, c) : s)
(.-) = SCons

infixr 5 .-

-- makes it easier to write
data SField (fn :: Symbol) (c :: Content) where
  SField :: KnownSymbol fn => SContent c -> SField fn c

deriving instance Show (SField fn c)

data SContent (c :: Content) where
  SFixed :: KnownNat n => SContent ( 'StrFixed n)
  SVariable :: SContent 'StrVariable
  SNested :: SSchema s -> SContent ( 'Nested s)

deriving instance Show (SContent c)

data SIndexedSchema (s :: IndexedSchema) where
  SIndexed :: KnownNat n => SSchema s -> SIndexedSchema '(n, s)

data SSchemaDB (ss :: [IndexedSchema]) where
  SDBNil :: SSchemaDB '[]
  SDBCons :: SIndexedSchema s -> SSchemaDB ss -> SSchemaDB (s : ss)

(.=) :: SIndexedSchema s -> SSchemaDB ss -> SSchemaDB (s : ss)
(.=) = SDBCons

infixr 5 .=

-- Can also be obtained from a package such as "some"
data Some (f :: a -> Type) where
  Some :: f a -> Some f

deriving instance (forall a. Show (f a)) => Show (Some f)

{- | Takes a plain schema (as it could trivially be parsed from a text file)
 to a type-indexed schema (but with an unknown index).
-}
liftSchema :: PlainSchema -> Either Error (Some SSchema)
liftSchema [] = pure (Some SNil)
liftSchema ((fn, c) : s) = do
  SomeSymbol (_ :: Proxy fn) <- pure (someSymbolVal fn)
  Some c' <- liftContent c
  Some s' <- liftSchema s
  pure (Some (SCons (SField @fn c') s'))

liftContent :: PlainContent -> Either Error (Some SContent)
liftContent (StrFixed l) = case someNatVal (fromIntegral l) of
  Nothing -> Left "negative field length in schema"
  Just (SomeNat (_ :: Proxy n)) -> pure (Some (SFixed @n))
liftContent StrVariable = pure (Some SVariable)
liftContent (Nested s) = do
  Some s' <- liftSchema s
  pure (Some (SNested s'))

data TIndexed (is :: IndexedSchema) where
  TIndexed :: KnownNat n => TValue s -> TIndexed '(n, s)

deriving instance Show (TIndexed is)

-- TODO: We should probably use a data structure here with at least logarithmic
-- lookup, rather than linear lookup.
--
-- However, for statically indexing the schema,
-- a list-like structure is easiest.
--
data TValue (s :: Schema) where
  TNil :: TValue '[]
  TCons :: TField c -> TValue s -> TValue ('(fn, c) : s)

deriving instance Show (TValue s)

data TField (c :: Content) where
  -- TODO: using String here is wrong; should be a fixed-length entity
  TFixed :: String -> TField ( 'StrFixed n)
  TVariable :: String -> TField 'StrVariable
  TNested :: TValue s -> TField ( 'Nested s)

deriving instance Show (TField c)

data TInDB (sdb :: [IndexedSchema]) where
  TInDB :: In is sdb -> TIndexed is -> TInDB sdb

deriving instance Show (TInDB sdb)

data In (x :: a) (xs :: [a]) where
  Z :: In x (x : xs)
  S :: In y xs -> In y (x : xs)

deriving instance Show (In x xs)

type Parser = Parsec Void String

parseSchema :: SSchema s -> Parser (TValue s)
parseSchema SNil = pure TNil
parseSchema (SCons (SField SFixed) schema0) = go schema0
 where
  -- the helper function is used to get access to the type n,
  -- which unfortunately we
  -- cannot pattern-match on directly ...
  go ::
    forall fn n s.
    (KnownSymbol fn, KnownNat n) =>
    SSchema s ->
    Parser (TValue ('(fn, 'StrFixed n) : s))
  go schema = do
    let l = fromInteger (natVal (Proxy @n))
    x <- replicateM l anySingle
    v <- parseSchema schema
    pure (TCons (TFixed x) v)
parseSchema (SCons (SField SVariable) schema) = do
  l <- read . (: []) <$> digitChar -- read should not fail for digitChar
  x <- replicateM l anySingle
  v <- parseSchema schema
  pure (TCons (TVariable x) v)
parseSchema (SCons (SField (SNested s)) schema) = do
  v <- parseSchema s
  v' <- parseSchema schema
  pure (TCons (TNested v) v')

parseIndexed :: SIndexedSchema is -> Parser (TIndexed is)
parseIndexed (SIndexed schema0) = go schema0
 where
  -- the helper function is used to get access to the type n,
  -- which unfortunately we
  -- cannot pattern-match on directly ...
  go :: forall n s. KnownNat n => SSchema s -> Parser (TIndexed '(n, s))
  go schema = do
    let i = show (natVal (Proxy @n))
    _ <- string i <?> ("schema identifier " <> i)
    v <- parseSchema schema
    pure (TIndexed v)

parseAnySchema :: SSchemaDB sdb -> Parser (TInDB sdb)
parseAnySchema SDBNil = fail "no matching schemas"
parseAnySchema (SDBCons is iss) =
  TInDB Z <$> parseIndexed is
    <|> (\(TInDB i x) -> (TInDB (S i) x)) <$> parseAnySchema iss

renderSchema :: SSchema s -> TValue s -> String
renderSchema SNil TNil = ""
renderSchema (SCons (SField SFixed) schema0) v0 = go schema0 v0
 where
  go ::
    forall fn n s'.
    (KnownSymbol fn, KnownNat n) =>
    SSchema s' ->
    TValue ('(fn, 'StrFixed n) : s') ->
    String
  go schema (TCons (TFixed x) v) = x <> renderSchema schema v
renderSchema (SCons (SField SVariable) schema0) v0 = go schema0 v0
 where
  go ::
    forall fn s'.
    KnownSymbol fn =>
    SSchema s' ->
    TValue ('(fn, 'StrVariable) : s') ->
    String
  go schema (TCons (TVariable x) v) =
    show (length x) <> x <> renderSchema schema v
renderSchema (SCons (SField (SNested s0)) schema0) v0 = go s0 schema0 v0
 where
  go ::
    forall fn s' s''.
    KnownSymbol fn =>
    SSchema s'' ->
    SSchema s' ->
    TValue ('(fn, 'Nested s'') : s') ->
    String
  go s schema (TCons (TNested x) v) =
    renderSchema s x <> renderSchema schema v

renderIndexed :: SIndexedSchema is -> TIndexed is -> String
renderIndexed (SIndexed schema0) v0 = go schema0 v0
 where
  go :: forall n s. KnownNat n => SSchema s -> TIndexed '(n, s) -> String
  go schema (TIndexed v) = show (natVal (Proxy @n)) <> renderSchema schema v

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

view :: Lens' s a -> s -> a
view l = getConst . l Const

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

fieldLens :: Lens' (TField c) (FieldType c)
fieldLens wrap (TFixed x) = TFixed <$> wrap x
fieldLens wrap (TVariable x) = TVariable <$> wrap x
fieldLens wrap (TNested s) = TNested <$> wrap s

type family FieldType (c :: Content) :: Type where
  FieldType ( 'StrFixed n) = String
  FieldType 'StrVariable = String
  FieldType ( 'Nested s) = TValue s

-- would be better with proper type error messages
class KnownSymbol fn => HasField (fn :: Symbol) (c :: Content) (s :: Schema) where
  access :: Lens' (TValue s) (FieldType c)

class HasFieldAux (b :: Bool) (fn :: Symbol) (fn' :: Symbol) (c :: Content) (c' :: Content) (s :: Schema) where
  accessAux :: Lens' (TValue ('(fn, c) : s)) (FieldType c')

instance (KnownSymbol fn, KnownSymbol fn', HasFieldAux (fn == fn') fn fn' c c' s) => HasField fn' c' ('(fn, c) : s) where
  access = accessAux @(fn == fn') @fn @fn' @c @c' @s

instance (fn ~ fn', c ~ c') => HasFieldAux 'True fn fn' c c' s where
  accessAux wrap (TCons c s) = (\x -> TCons x s) <$> fieldLens wrap c

instance HasField fn' c' s => HasFieldAux 'False fn fn' c c' s where
  accessAux wrap (TCons c s) = TCons c <$> access @fn' @c' @s wrap s

indexed :: Lens' (TIndexed '(n, s)) (TValue s)
indexed wrap (TIndexed x) = TIndexed <$> wrap x

unwrapField :: TField c -> FieldType c
unwrapField (TFixed x) = x
unwrapField (TVariable x) = x
unwrapField (TNested s) = s

rewrapField :: (FieldType c -> FieldType c) -> TField c -> TField c
rewrapField f (TFixed x) = TFixed (f x)
rewrapField f (TVariable x) = TVariable (f x)
rewrapField f (TNested s) = TNested (f s)

-- Re-export parsing function, so that app does not need megaparsec
parseMsg :: Parsec e s a -> String -> s -> Either (ParseErrorBundle s e) a
parseMsg = parse
