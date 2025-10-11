module Flow.AST.Common where

import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "text" Data.Text (Text)

-- | represents module path chunks like std, io, IO, etc.Do
-- could start either with a capital letter or a lowercase letter
newtype ModuleIdentifier = ModuleIdentifier Text
  deriving (Eq, Ord, Show)

-- | represents type names like A, Option, Result, Functor, etc. always starts with a capital letter
-- as well can represent name of Constructor like Some, Ok, Err, etc.
newtype SimpleTypeIdentifier = SimpleTypeIdentifier Text
  deriving (Eq, Ord, Show)

-- | represents variable names like x, y, z, etc. always starts with a lowercase letter
newtype SimpleVarIdentifier = SimpleVarIdentifier Text
  deriving (Eq, Ord, Show)

data AnyTypeIdentifier = AnyTypeIdentifier
  { qualifier :: NonEmptyVector ModuleIdentifier
  , identifier :: SimpleTypeIdentifier
  }
  deriving (Eq, Ord, Show)

data AnyVarIdentifier = AnyVarIdentifier
  { qualifier :: NonEmptyVector ModuleIdentifier
  , identifier :: SimpleVarIdentifier
  }
  deriving (Eq, Ord, Show)

newtype Scope = Scope Text
  deriving (Eq, Ord, Show)
