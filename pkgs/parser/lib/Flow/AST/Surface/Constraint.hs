{-# LANGUAGE TypeFamilies #-}

module Flow.AST.Surface.Constraint where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (ModuleIdentifier, ScopeIdentifier, SimpleTypeIdentifier, SimpleVarIdentifier)

data AnyTypeIdentifier ty ann = AnyTypeIdentifier
  { qualifier :: Maybe (NonEmptyVector (ModuleIdentifier ann))
  , typeQualifier :: Maybe (TypeQualifierF ty ann)
  , identifier :: SimpleTypeIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeQualifierF ty ann = TypeQualifierF
  { typeName :: SimpleTypeIdentifier ann
  , typeParams :: BindersAppF ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AnyVarIdentifier ty ann = AnyVarIdentifier
  { qualifier :: Maybe (NonEmptyVector (ModuleIdentifier ann))
  , typeQualifier :: Maybe (TypeQualifierF ty ann)
  , identifier :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BindersF scopeBinder typeBinder ty ann = BindersF
  { scopes :: Vector (scopeBinder ty ann)
  , types :: Vector (typeBinder ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type BindersAppF = BindersF ScopeBinderWoConstraintsF BinderAppF
type BindersWConstraintsF = BindersF ScopeBinderWConstraintsF BinderWConstraintsF
type BindersWoConstraintsF = BindersF ScopeBinderWoConstraintsF BinderWoConstraintsF

newtype BinderAppF ty ann = BinderAppF {ty :: ty ann}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

data ScopeBinderWConstraintsF ty ann = ScopeBinderWConstraintsF
  { name :: ScopeIdentifier ann
  , constraint :: Maybe (BinderConstraintsF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

newtype ScopeBinderWoConstraintsF ty ann = ScopeBinderWoConstraintsF (ScopeIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

data BinderWConstraintsF ty ann = BinderWConstraintsF -- A | A :< Monoid
  { name :: SimpleTypeIdentifier ann
  , kindShort :: Maybe (KindTreeRootF ty ann)
  , typeType :: Maybe (ty ann)
  , constraint :: Maybe (BinderConstraintsF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BinderConstraintsF ty ann = BinderConstraintsF
  { constraints :: NonEmptyVector (AnyTypeIdentifier ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BinderWoConstraintsF ty ann = BinderWoConstraintF
  { name :: SimpleTypeIdentifier ann
  , kindShort :: Maybe (KindTreeRootF ty ann)
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeDefinitionF ty ann = TypeDefinitionF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhereClauseF ty ann -- type X = Y | Functor<A> | etc
  = WhereConstraintF (ty ann)
  | WhereAliasF (TypeDefinitionF ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhereBlockF ty ann = WhereBlockF
  { clauses :: NonEmptyVector (WhereClauseF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type KindTreeRootF ty ann = NonEmptyVector (KindTreeF ty ann) -- <_, _<_>>

data KindTreeF ty ann
  = KTHoleF (KindHoleF ty ann) -- _ | _: Type
  | KTParamsF (KindParamsF ty ann) -- _<_, _>
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data KindHoleF ty ann = KindHoleF
  { holeAnn :: ann
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data KindParamsF ty ann = KindParamsF
  { holeAnn :: ann
  , params :: NonEmptyVector (KindTreeF ty ann)
  , typeType :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
