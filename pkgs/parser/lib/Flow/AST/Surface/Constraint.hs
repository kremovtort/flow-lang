module Flow.AST.Surface.Constraint where

import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Common (SimpleTypeIdentifier, AnyTypeIdentifier, ScopeIdentifier)

data BinderF ty ann = BinderF -- A | A :< Monoid
  { name :: SimpleTypeIdentifier ann
  , type_ :: ty ann
  , constraint :: Vector (AnyTypeIdentifier ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BinderWoConstraintF ty ann = BinderWoConstraint
  { name :: SimpleTypeIdentifier ann
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeDefinitionF ty ann = TypeDefinitionF
  { name :: SimpleTypeIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderWoConstraintF ty ann)
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WhereClauseF ty ann -- type X = Y | Functor<A> | etc
  = WhereConstraintF (ty ann) ann
  | WhereAliasF (TypeDefinitionF ty ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
