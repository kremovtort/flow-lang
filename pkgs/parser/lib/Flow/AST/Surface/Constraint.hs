{-# LANGUAGE TypeFamilies #-}

module Flow.AST.Surface.Constraint where

import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (AnyTypeIdentifier, ScopeIdentifier, SimpleTypeIdentifier)

data BindersF scopeBinder typeBinder ty ann = BindersF
  { scopes :: Vector (scopeBinder ty ann)
  , types :: Vector (typeBinder ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type BindersWConstraintsF = BindersF ScopeBinderWConstraintsF BinderWConstraintsF
type BindersWoConstraintsF = BindersF ScopeBinderWoConstraintsF BinderWoConstraintsF

data ScopeBinderWConstraintsF ty ann = ScopeBinderWConstraintsF
  { name :: ScopeIdentifier ann
  , constraint :: Maybe (BinderConstraintsF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype ScopeBinderWoConstraintsF ty ann = ScopeBinderWoConstraintsF (ScopeIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BinderWConstraintsF ty ann = BinderWConstraintsF -- A | A :< Monoid
  { name :: SimpleTypeIdentifier ann
  , typeType :: Maybe (ty ann, ann)
  , constraint :: Maybe (BinderConstraintsF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BinderConstraintsF ty ann = BinderConstraintsF
  { constraints :: NonEmptyVector (AnyTypeIdentifier ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BinderWoConstraintsF ty ann = BinderWoConstraintF
  { name :: SimpleTypeIdentifier ann
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeDefinitionF ty ann = TypeDefinitionF
  { name :: SimpleTypeIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderWoConstraintsF ty ann)
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WhereClauseF ty ann -- type X = Y | Functor<A> | etc
  = WhereConstraintF (ty ann) ann
  | WhereAliasF (TypeDefinitionF ty ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WhereBlockF ty ann = WhereBlockF
  { constraints :: NonEmptyVector (WhereClauseF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
