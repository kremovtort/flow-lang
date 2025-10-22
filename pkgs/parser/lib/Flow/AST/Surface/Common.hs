{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.AST.Surface.Common where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "vector" Data.Vector (Vector)
import "tree-diff" Data.TreeDiff.Class (ToExpr (toExpr))

{- | represents module path chunks like std, io, IO, etc.Do
could start either with a capital letter or a lowercase letter
-}
data ModuleIdentifier ann = ModuleIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

{- | represents type names like A, Option, Result, Functor, etc. always starts with a capital letter
as well can represent name of Constructor like Some, Ok, Err, etc.
-}
data SimpleTypeIdentifier ann = SimpleTypeIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- | represents variable names like x, y, z, etc. always starts with a lowercase letter
data SimpleVarIdentifier ann = SimpleVarIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AnyTypeIdentifier ann = AnyTypeIdentifier
  { qualifier :: Vector (ModuleIdentifier ann)
  , qualifierAnn :: Maybe ann
  , identifier :: SimpleTypeIdentifier ann
  , identifierAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AnyVarIdentifier ann = AnyVarIdentifier
  { qualifier :: Vector (ModuleIdentifier ann)
  , qualifierAnn :: Maybe ann
  , identifier :: SimpleVarIdentifier ann
  , identifierAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ScopeIdentifier ann = ScopeIdentifier
  { name :: Text
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

instance (ToExpr a) => ToExpr (NonEmptyVector a) where
  toExpr = toExpr . NonEmptyVector.toList
