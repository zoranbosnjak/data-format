{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits

----------------------------------------

-- General overview:
--  - Each 'schema' is a distinct type.
--  - In the real environment, the schemas will be predefined/generated,
--    for example in the 'Specs.hs' module which will be imported in the
--    eventual application. For this test file, the schemas are inlined
--    in the last half of this file.
--
-- Scheam structure:
--
-- Element is a leaf of the structure. It will contain actual values.
--
-- Group is a 'group of (sub)items'. All subitems have to be defined/present,
-- like in tuple or product type. Item has a name (Symbol) and contains a
-- value of 'the' structure... or it's Spare (see 'data Item' below)
--
-- Extended is a 'group of groups (of subitems)', where (in runtime) the first
-- groups is normally present, other groups are optional. Second group must be
-- present before the third group and so on (like non-empty list).
-- When constructing an 'Extended' value, we want to keep track at the type
-- level which groups are actually present. We might also want to 'extend'
-- the existing value with new group or 'drop' groups from the value.
-- Problems/questions:
--  - The Schema (Specs.hs) will define a primary group and all extensions.
--    It is up to the application which extensions will be used.
--    How to specify this at the type level? Currently the 'Ext' type level
--    function is used to specify how many extensions are present.
--    Is there any better way?
--  - The 'Extended' might be nested (and present many times) in the
--    eventual 'Record' structure. If Extended will require a type parameter,
--    how to handle it in the Record? Existentials??
--  - How to define 'mkExtended', to simplify value construction?
--    We want to take a Group and turn it into 'group of groups'. It shall
--    fail at compile time if the given Group does not match the target.
--    That is: if it's not possible to form complete groups.
--
-- Repetitive is a normal 'list' (not important at the moment).
--
-- Explicit (not important at the moment)
--
-- Compound is like a 'Group', except:
--  - empty slots are possible in the structure (Maybe at the type level)
--  - in the non-empty slots, the value is optionally present (Maybe at runtime)
--
-- The eventual value (called 'Record') is a nested 'Compound' type.
-- See 'type SchemaRecord' below. A set of those will be defined
-- in 'Specs.hs'.
--

data TFspec
    = FSpecFx           -- Fspec with FX bit extensions
    | FSpecFixed Nat    -- Predefined fix-size fspec

data TVariation
    = TElement
    | TGroup [TItem]
    | TExtended [[TItem]] [[TItem]]
    | TRepetitive Nat TVariation
    | TExplicit (Maybe TVariation)
    | TCompound TFspec [Maybe TItem]

data Variation (t :: TVariation) where
    Element     :: Variation 'TElement
    Group       :: GList ts -> Variation ('TGroup ts)
    Extended    :: EList act ext -> Variation ('TExtended act ext)
    Repetitive  :: [Variation t] -> Variation ('TRepetitive n t)
    Explicit    :: ExplicitCont t -> Variation ('TExplicit t)
    Compound    :: CList ts -> Variation ('TCompound mn ts)

-- Question: There are lot of trivial Eq definitions.
-- Is it possible to simplify/derive them?

instance Eq (Variation 'TElement) where
    Element == Element = True

instance Eq (GList ts) => Eq (Variation ('TGroup ts)) where
    Group val1 == Group val2 = val1 == val2

instance Eq (EList act ext) => Eq (Variation ('TExtended act ext)) where
    Extended val1 == Extended val2 = val1 == val2

instance Eq (Variation t) => Eq (Variation ('TRepetitive n t)) where
    Repetitive lst1 == Repetitive lst2 = lst1 == lst2

instance Eq (ExplicitCont t) => Eq (Variation ('TExplicit t)) where
    Explicit val1 == Explicit val2 = val1 == val2

instance Eq (CList ts) => Eq (Variation ('TCompound mn ts)) where
    Compound val1 == Compound val2 = val1 == val2

----------------------------------------

-- will be defined later
data BitString = BitString
    deriving (Eq)

----------------------------------------

type ItemName = Symbol

data TItem
    = TSpare Nat
    | TItem ItemName TVariation

data Item (t :: TItem) where
    Spare :: Item ('TSpare n)
    -- Question: Should there be 'KnownSymbol'?
    -- Item  :: forall name t. KnownSymbol name => Variation t -> Item ('TItem name t)
    Item  :: forall name t. Variation t -> Item ('TItem name t)

instance Eq (Item ('TSpare n)) where
    Spare == Spare = True

instance Eq (Variation var) => Eq (Item ('TItem name var)) where
    Item var1 == Item var2 = var1 == var2

-- Question:
-- There are GList/EList/CList helper structures.
-- Is it possible to unify them, to write the instances once?
-- (Not sure if the EList is optimal structure for the Extended).

----------------------------------------
-- GList

data GList (ts :: [TItem]) where
    GNil  :: GList '[]
    GCons :: Item t -> GList ts -> GList (t ': ts)

instance Eq (GList '[]) where
    GNil == GNil = True

instance (Eq (Item t), Eq (GList ts)) => Eq (GList (t ': ts)) where
    GCons a as == GCons b bs = a == b && as == bs

----------------------------------------
-- EList

data EList (act :: [[TItem]]) (ext :: [[TItem]]) where
    ENil :: EList '[] ext
    EExt :: GList t -> EList spec (t ': ext) -> EList (t ': spec) ext

instance Eq (EList '[] ext) where
    ENil == ENil = True

instance (Eq (GList t), Eq (EList ts (t ': ext))) => Eq (EList (t ': ts) ext) where
    EExt a as == EExt b bs = a == b && as == bs

----------------------------------------
-- Explicit content

data ExplicitCont (t :: (Maybe TVariation)) where
    ExplicitRaw :: BitString -> ExplicitCont 'Nothing
    ExplicitExpanded :: Variation vt -> ExplicitCont ('Just vt)

instance Eq BitString => Eq (ExplicitCont 'Nothing) where
    ExplicitRaw val1 == ExplicitRaw val2 = val1 == val2

instance Eq (Variation vt) => Eq (ExplicitCont ('Just vt)) where
    ExplicitExpanded val1 == ExplicitExpanded val2 = val1 == val2

----------------------------------------
-- CList

data CList (ts :: [Maybe TItem]) where
    CNil  :: CList '[]
    CConsNone :: CList ts -> CList ('Nothing ': ts)
    CConsItem :: Maybe (Item t) -> CList ts -> CList (('Just t) ': ts)

instance Eq (CList '[]) where
    CNil == CNil = True

instance Eq (CList ts) => Eq (CList ('Nothing ': ts)) where
    CConsNone val1 == CConsNone val2 = val1 == val2

instance (Eq (Item t), Eq (CList ts)) => Eq (CList ('Just t ': ts)) where
    CConsItem a as == CConsItem b bs = a == b && as == bs

----------------------------------------
-- Element handling

mkElement :: Variation 'TElement
mkElement = Element

----------------------------------------
-- Group handling

mkGroup :: GList ts -> Variation ('TGroup ts)
mkGroup = Group

----------------------------------------
-- Extended handling

emptyExtended :: Variation ('TExtended '[] ext)
emptyExtended = Extended ENil

-- | How many extensions of the original definition we want
type family Ext (n :: Nat) (t :: TVariation) :: TVariation where
    Ext 0 ('TExtended a b) = ('TExtended a b)
    Ext n ('TExtended a (b ': bs)) = Ext (n-1) ('TExtended (b ': a) bs)

extend :: GList t -> Variation ('TExtended act (t ': ext)) -> Variation ('TExtended (t ': act) ext)
extend gLst (Extended eLst) = Extended (EExt gLst eLst)

mkExtended :: a
mkExtended = undefined

----------------------------------------
-- Repetitive handling

mkRepetitive :: [Variation t] -> Variation ('TRepetitive n t)
mkRepetitive = Repetitive

----------------------------------------
-- Explicit handling

mkExplicitRaw :: BitString -> Variation ('TExplicit 'Nothing)
mkExplicitRaw = Explicit . ExplicitRaw

mkExplicitExpanded :: Variation vt -> Variation ('TExplicit ('Just vt))
mkExplicitExpanded = Explicit . ExplicitExpanded

----------------------------------------
-- Compound handling

class EmptyCompound ts where
    emptyCompound :: Variation ('TCompound mn ts)

instance EmptyCompound '[] where
    emptyCompound = Compound CNil

instance EmptyCompound ts => EmptyCompound ('Nothing ': ts) where
    emptyCompound = Compound (CConsNone rest) where
        Compound rest = emptyCompound

instance EmptyCompound ts => EmptyCompound ('Just t ': ts) where
    emptyCompound = Compound (CConsItem Nothing rest) where
        Compound rest = emptyCompound

-- Question: Is it better to get/set subitem with (Proxy @iName) argument?
class IsCompound (iName :: ItemName) t ts | ts iName -> t where
    -- TODO: getCompound :: -> Variation ('TCompound mn ts) -> Maybe (Variation t)
    setCompound :: Maybe (Variation t) -> Variation ('TCompound mn ts) -> Variation ('TCompound mn ts)

instance IsCompound iName t ts => IsCompound iName t ('Nothing ': ts) where
    setCompound val (Compound (CConsNone rest)) =
        case setCompound @iName val (Compound rest) of
            Compound result -> Compound (CConsNone result)

instance {-# OVERLAPPING #-} IsCompound iName t ('Just ('TItem iName t) ': ts) where
    setCompound val (Compound (CConsItem _oldVal rest)) = Compound (CConsItem (fmap Item val) rest)

instance IsCompound iName t ts => IsCompound iName t ('Just ('TItem a b) ': ts) where
    setCompound val (Compound (CConsItem oldVal rest)) =
        case setCompound @iName val (Compound rest) of
            Compound result -> Compound (CConsItem oldVal result)

class MkCompound as ts where
    mkCompound :: GList as -> Variation ('TCompound mn ts)

instance EmptyCompound ts => MkCompound '[] ts where
    mkCompound GNil = emptyCompound

instance (IsCompound name t ts, MkCompound as ts) => MkCompound (('TItem name t) ': as) ts where
    mkCompound (GCons (Item v) is) = setCompound @name (Just v) (mkCompound is)

----------------------------------------

nil :: GList '[]
nil = GNil

infixr 5 &:
(&:) :: Item t -> GList ts -> GList (t ': ts)
(&:) = GCons




----------------------------------------
-- End of generic library code,
-- Start of application code examples...
--




-- Below, we define some lists of values, which are expected to be equal.
-- Example equaliti check:
-- ghci> areEqual vElement
areEqual :: Eq a => [a] -> Bool
areEqual [] = True
areEqual (_:[]) = True
areEqual (a:b:lst) = (a == b) && areEqual (b:lst)

----------------------------------------
type SchemaElement = 'TElement

vElement :: [Variation SchemaElement]
vElement =
    [ Element
    , mkElement
    ]

----------------------------------------
type SchemaGroup = 'TGroup
   '[ 'TItem "S1" 'TElement
    , 'TItem "S2" 'TElement
    ]

vGroup :: [Variation SchemaGroup]
vGroup =
    [ Group (GCons (Item @"S1" Element) (GCons (Item @"S2" Element) GNil))
    , Group (GCons (Item Element) (GCons (Item Element) GNil))
    , mkGroup
        ( Item @"S1" Element
       &: Item @"S2" Element
       &: nil)
    , mkGroup
        ( Item Element
       &: Item Element
       &: nil)
    ]

----------------------------------------
type SchemaExtended = 'TExtended
   '[]
   '[
       '[ 'TItem "S1" 'TElement
        , 'TItem "S2" 'TElement
        ]
    ,  '[ 'TItem "S3" 'TElement
        , 'TItem "S4" 'TElement
        ]
    ,  '[ 'TItem "S5" 'TElement
        , 'TItem "S6" 'TElement
        , 'TItem "S7" 'TElement
        ]
    ]

vExtended :: [Variation (Ext 2 SchemaExtended)]
vExtended =
    [ Extended (EExt (GCons (Item @"S3" Element) (GCons (Item @"S4" Element) GNil))
        (EExt (GCons (Item @"S1" Element) (GCons (Item @"S2" Element) GNil)) ENil))
    , Extended (EExt (GCons (Item Element) (GCons (Item Element) GNil))
        (EExt (GCons (Item Element) (GCons (Item Element) GNil)) ENil))
    , let
        ext0 =
            ( Item @"S1" Element
           &: Item @"S2" Element
           &: nil)
        ext1 =
            ( Item @"S3" Element
           &: Item @"S4" Element
           &: nil)
      in extend ext1 $ extend ext0 emptyExtended
    , let
        ext0 =
            ( Item Element
           &: Item Element
           &: nil)
        ext1 =
            ( Item Element
           &: Item Element
           &: nil)
      in extend ext1 $ extend ext0 emptyExtended
    , mkExtended
        ( Item @"S1" Element
       &: Item @"S2" Element
       -- extension1 (optional)
       &: Item @"S3" Element
       &: Item @"S4" Element
       -- extension2 (optional)
       &: nil)
    , mkExtended
        ( Item Element
       &: Item Element
       -- extension1 (optional)
       &: Item Element
       &: Item Element
       -- extension2 (optional)
       &: nil)
    ]

----------------------------------------
type SchemaRepetitive = 'TRepetitive 8 'TElement

vRepetitive :: [Variation SchemaRepetitive]
vRepetitive =
    [ Repetitive [Element, Element]
    , mkRepetitive [Element, Element]
    ]

----------------------------------------
type SchemaExplicitRaw = 'TExplicit 'Nothing

vExplicitRaw :: [Variation SchemaExplicitRaw]
vExplicitRaw =
    [ Explicit $ ExplicitRaw BitString
    , mkExplicitRaw BitString
    ]

type SchemaExplicitExpanded = 'TExplicit ('Just 'TElement)

vExplicitExpanded :: [Variation SchemaExplicitExpanded]
vExplicitExpanded =
    [ Explicit $ ExplicitExpanded Element
    , mkExplicitExpanded Element
    ]

----------------------------------------
type SchemaCompound = 'TCompound 'FSpecFx
   '[ 'Just ('TItem "I1" 'TElement)
    , 'Nothing
    , 'Just ('TItem "I2" 'TElement)
    ]

vCompound :: [Variation SchemaCompound]
vCompound =
    [ Compound (CConsItem (Just (Item @"I1" Element)) (CConsNone (CConsItem Nothing CNil)))
    , Compound (CConsItem (Just (Item Element)) (CConsNone (CConsItem Nothing CNil)))
    , setCompound @"I1" (Just Element)
      $ setCompound @"I2" Nothing   -- optional, does not change anything
      $ emptyCompound
    , mkCompound
        -- TODO: name is required, need better error message if name is not given
        ( Item @"I1" Element
       &: nil)
    ]

----------------------------------------
--
-- Something like this (set of those) will be defined in the 'Specs.hs',
-- but temporary, just define some example Schema here...
--
type SchemaRecord = 'TCompound 'FSpecFx

   '[ 'Just ('TItem "I1" 'TElement)         -- Item1

    , 'Just ('TItem "I2" ('TGroup           -- Item2
       '[ 'TItem "S1" 'TElement                 -- Subitem
        , 'TItem "S2" 'TElement                 -- Subitem
        ]))

    {-
       Extended??
    , 'Just ('TItem "I3" ('TExtended        -- Item3

       '[  '[ 'TItem "S1" 'TElement     -- primary part
            , 'TItem "S2" 'TElement
            ]
        ,  '[ 'TItem "S3" 'TElement     -- extension
            , 'TItem "S4" 'TElement
            , 'TItem "S5" 'TElement
            ]
        ,  '[ 'TItem "S6" 'TElement     -- extension
            , 'TItem "S7" 'TElement
            ]
        ]))
    -}

    , 'Just ('TItem "I4" ('TRepetitive 8    -- Item4
        ('TGroup
           '[ 'TItem "S1" 'TElement
            , 'TItem "S2" 'TElement
            ])))

    , 'Nothing                              -- empty slot

    , 'Just ('TItem "I5" ('TCompound 'FSpecFx -- Item5
       '[ 'Just ('TItem "S1" 'TElement)
        , 'Nothing
        , 'Just ('TItem "S2" 'TElement)
       ]))
    ]

{-
-- Eventual value will be created like this:
vRecord :: Variation SchemaRecord
vRecord = mkCompound
    ( Item @"I1" Element
   &: Item @"I2" item2
   &: Item @"I3" item3
   &: Item @"I4" item4
   &: Item @"I5" item5
   &: nil)

  where

    item2 = mkGroup
       ( Item @"S1" Element
       &: Item @"S2" Element
       &: nil
        )

    -- Remark: some subset of extensions are actually used

    item3 = mkExtended
        ( Item @"S1" Element
       &: Item @"S2" Element
       -- Below this part is optional
       -- In this example, the S3, S4 and S5 are present
       &: Item @"S3" Element
       &: Item @"S4" Element
       &: Item @"S5" Element
       -- Below this part is optional again
       -- In this example, S6 and S7 are not present
       --
       &: nil)

    item4 = mkRepetitive
        [ repItem
        , repItem
        , repItem
        ]
      where
        repItem = mkGroup
            ( Item @"S1" Element
           &: Item @"S2" Element
           &: nil
            )

    item5 = mkCompound
        ( Item @"S1" Element
       &: Item @"S2" Element
       &: nil)
-}

