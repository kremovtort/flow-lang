module Flow.AST.Surface.Use where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (ModuleIdentifier, SimpleTypeIdentifier, SimpleVarIdentifier)

data UseClause ann = UseClause
  { root :: ModuleIdentifier ann
  , tree :: Maybe (UseTree ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTree ann
  = UseTrBranch (ModuleIdentifier ann) (UseTree ann)
  | UseTrNested (Vector (UseTree ann))
  | UseTrLeafWildcard ann
  | UseTrLeafVar (UseTreeLeaf SimpleVarIdentifier ann)
  | UseTrLeafType (UseTreeLeaf SimpleTypeIdentifier ann)
  | UseTrLeafMethod (UseTreeLeaf SimpleVarIdentifier ann)
  | UseTrLeafMethodAsFn (UseTreeLeafMethodAsFn ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTreeLeaf f ann = UseTreeLeaf
  { use :: f ann
  , as :: Maybe (f ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTreeLeafMethodAsFn ann = UseTreeLeafMethodAsFn
  { use :: SimpleVarIdentifier ann
  , as :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
