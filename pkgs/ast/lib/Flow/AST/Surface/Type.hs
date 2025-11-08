module Flow.AST.Surface.Type where

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (RegionIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (AnyTypeIdentifier, BindersAppF, BindersWConstraintsF, WhereBlockF)
import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

data TypeF ty ann
  = TyWildcardF -- _
  | TyBuiltinF Builtin
  | TyIdentifierF (AnyTypeIdentifier ty ann) -- MyType
  | TyParensF (ty ann) -- (A)
  | TyAppF (AppF ty ann) -- Option<A>
  | TyTupleF (NonEmptyVector (ty ann)) -- (A, B, C)
  | TyRefAppF (RefF ann) (ty ann) -- &'s T | &'s mut T | &T | &mut T
  | TyRefF (RefF ann) -- &'s | &'s mut | & | &mut / higher kind reference
  | TyForallF (ForallF ty ann) -- <A :< Monoid> fn(List<A>) -> A
  | TyFnF (FnF ty ann) -- fn(List<A>) -> A
  | TyEffectRowF (EffectRowF ty ann) -- @[IO, State<S>] | @['s, IO] | @['s, IO, s: State<S>, ..R] | etc
  | TyEquals (ty ann) (ty ann) -- A == B
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Builtin
  = BuiltinUnit
  | BuiltinNever
  | BuiltinBool
  | BuiltinI8
  | BuiltinI16
  | BuiltinI32
  | BuiltinI64
  | BuiltinI128
  | BuiltinISize
  | BuiltinU8
  | BuiltinU16
  | BuiltinU32
  | BuiltinU64
  | BuiltinU128
  | BuiltinUSize
  | BuiltinF32
  | BuiltinF64
  | BuiltinF128
  | BuiltinByte
  | BuiltinByteString
  | BuiltinChar
  | BuiltinString
  deriving (Eq, Ord, Show, Generic, ToExpr)

data AppF ty ann = AppF -- Option<A>
  { head :: ty ann
  , args :: BindersAppF ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data RefF ann = RefF -- &'s T | &'s mut T | &T | &mut T
  { region :: Maybe (RegionIdentifier ann)
  , mutability :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ForallF ty ann = ForallF -- <A :< Monoid> fn(List<A>) -> A
  { params :: BindersWConstraintsF ty ann
  , result :: ty ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnF ty ann = FnF -- fn(List<A>) -> A
  { args :: Vector (ty ann)
  , effectsResult :: Maybe (FnEffectsResultF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectsResultF ty ann = FnEffectsResultF
  { effects :: Maybe (FnEffectsF ty ann)
  , result :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectsF ty ann
  = FnEffectsTypeF (ty ann)
  | FnEffectsRowF (FnEffectRowF ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectRowF ty ann = FnEffectRowF
  { regions :: Vector (RegionIdentifier ann)
  , effects :: Vector (FnEffectAtomF ty ann)
  , tailVars :: Vector (ty ann, ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnEffectAtomF ty ann
  = FnEffectAtomTypeF (ty ann) -- E1
  | FnEffectAtomNameTypeF (SimpleVarIdentifier ann) (ty ann) -- e1: E1
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectRowF ty ann = EffectRowF
  { regions :: Vector (RegionIdentifier ann)
  , effects :: Vector (ty ann)
  , tailVars :: Vector (ty ann, ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
