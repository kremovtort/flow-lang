module Flow.AST.Common where

import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "text" Data.Text (Text)

-- | represents module path chunks like std, io, IO, etc.Do
-- could start either with a capital letter or a lowercase letter
data ModuleIdentifier ann = ModuleIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | represents type names like A, Option, Result, Functor, etc. always starts with a capital letter
-- as well can represent name of Constructor like Some, Ok, Err, etc.
data SimpleTypeIdentifier ann = SimpleTypeIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | represents variable names like x, y, z, etc. always starts with a lowercase letter
data SimpleVarIdentifier ann = SimpleVarIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AnyTypeIdentifier ann = AnyTypeIdentifier
  { qualifier :: NonEmptyVector (ModuleIdentifier ann)
  , identifier :: SimpleTypeIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AnyVarIdentifier ann = AnyVarIdentifier
  { qualifier :: NonEmptyVector (ModuleIdentifier ann)
  , identifier :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ScopeIdentifier ann = ScopeIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
