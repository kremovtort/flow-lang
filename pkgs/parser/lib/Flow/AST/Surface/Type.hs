module Flow.AST.Surface.Type where

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (AnyTypeIdentifier, ScopeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (BindersWConstraintsF, WhereBlockF)
import "vector" Data.Vector (Vector)

data TypeF ty ann
  = TyBuiltinF Builtin ann
  | TyIdentifierF (AnyTypeIdentifier ann) -- MyType
  | TyAppF (AppF ty ann) -- Option<A>
  | TyTupleF (NonEmptyVector (ty ann)) ann -- (A, B, C)
  | TyRefF (RefF ann) -- &'s T | &'s mut T | &T | &mut T
  | TyForallF (ForallF ty ann) -- <A :< Monoid> fn(List<A>) -> A
  | TyFnF (FnF ty ann) -- fn(List<A>) -> A
  | TyEffectRowF (EffectRowF ty ann) -- @[IO, State<S>] | @['s, IO] | @['s, IO, s: State<S>, ..R] | etc
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Builtin
  = BuiltinUnit
  | BuiltinNever
  | BuiltinBool
  | BuiltinI8
  | BuiltinI16
  | BuiltinI32
  | BuiltinI64
  | BuiltinI128
  | BuiltinU8
  | BuiltinU16
  | BuiltinU32
  | BuiltinU64
  | BuiltinU128
  | BuiltinF32
  | BuiltinF64
  | BuiltinF128
  | BuiltinByte
  | BuiltinByteString
  | BuiltinChar
  | BuiltinString
  deriving (Eq, Ord, Show)

data AppF ty ann = AppF -- Option<A>
  { head :: ty ann
  , headAnn :: ann
  , args :: NonEmptyVector (ty ann)
  , argsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data RefF ann = RefF -- &'s T | &'s mut T | &T | &mut T
  { scope :: Maybe (ScopeIdentifier ann)
  , mutability :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ForallF ty ann = ForallF -- <A :< Monoid> fn(List<A>) -> A
  { params :: BindersWConstraintsF ty ann
  , paramsAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , whereAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data FnF ty ann = FnF -- fn(List<A>) -> A
  { args :: Vector (ty ann)
  , argsAnn :: ann
  , effects :: Maybe (ty ann, ann)
  , result :: ty ann
  , resultAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EffectRowF ty ann = EffectRowF
  { effects :: Vector (EffectAtomF ty ann)
  , tailVar :: Maybe (AnyTypeIdentifier ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EffectAtomF ty ann
  = EAtomTypeF (ty ann) ann -- E1
  | EAtomNameTypeF (SimpleVarIdentifier ann) (ty ann) ann -- e1: E1
  | EAtomScopeF (ScopeIdentifier ann) ann -- 's
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
