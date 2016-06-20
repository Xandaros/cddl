{-# LANGUAGE FlexibleInstances #-}
module Data.CDDL.Types where

import Data.List.NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Semigroup

type Identifier = Text.Text

type Typename = Identifier
type Groupname = Identifier

data CDDL = CDDL Typename Rules
    deriving (Show)

data Rules = Rules (Map.Map Typename Type) (Map.Map Groupname Group)
    deriving (Show)
-- Semigroup

data Type = LiteralType Literal
          | ChoiceType [Type] -- ∀n. Vector (S (S n)) Type
          | RangeType Int Int Bool
          | ListType Group
          | StructType Group
          | ReferencedType Typename
          | RawType Int Int
          | TaggedType Int Type
          | AnyType
          deriving (Show)
        -- One possible Monoid instances - good idea?

data Group = GroupProduct (NonEmpty Group)
           | GroupChoice (NonEmpty Group)
           | GroupReference Groupname
           deriving (Show)
        -- Two possible Monoid instances - no Monoid

data Literal = NumberLiteral Double
             | StringLiteral Text.Text
             deriving (Show)


-- TODO: Add special cases where grp1 or grp2 ere GroupChoice/GroupProduct
instance Semigroup (Sum Group) where
    (Sum grp1) <> (Sum grp2) = Sum $ GroupChoice (grp1 :| [grp2])

instance Semigroup (Product Group) where
    (Product grp1) <> (Product grp2) = Product $ GroupProduct (grp1 :| [grp2])
