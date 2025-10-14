module Flow.AST.Surface.Type where

import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (ScopeIdentifier, AnyTypeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (BinderF, WhereClauseF)

data TypeF ty ann
  = TyBuiltinF Builtin ann
  | TyIdentifierF (AnyTypeIdentifier ann) -- MyType
  | TyAppF (AppF ty ann) -- Option<A>
  | TyTupleF (Vector (ty ann)) ann -- (A, B, C)
  | TyRefF (RefF ty ann) -- &'s T | &'s mut T | &T | &mut T
  | TyForallF (ForallF ty ann) -- <A :< Monoid> fn(List<A>) -> A
  | TyFnF (FnF ty ann) -- fn(List<A>) -> A
  | TyEffectRowF (EffectRowF ty ann) -- [IO, State<S>] | ['s, IO], | [IO, ..R, ..S, ..] | etc
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
  , args :: Vector (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data RefF ty ann = RefF -- &'s T | &'s mut T | &T | &mut T
  { scope :: Maybe (ScopeIdentifier ann)
  , mutability :: Bool
  , inner :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ForallF ty ann = ForallF -- <A :< Monoid> fn(List<A>) -> A
  { scopes :: Vector (ScopeIdentifier ann)
  , binders :: Vector (BinderF ty ann)
  , result :: ty ann
  , whereClauses :: Vector (WhereClauseF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data FnF ty ann = FnF -- fn(List<A>) -> A
  { args :: Vector (ty ann)
  , effects :: Maybe (ty ann)
  , result :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EffectRowF ty ann = EffectRowF
  { effects :: Vector (EffectAtomF ty ann)
  , tailVar :: Maybe (SimpleVarIdentifier ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EffectAtomF ty ann
  = EAtomNameF (SimpleVarIdentifier ann) ann -- e1
  | EAtomTypeF (ty ann) ann -- E1
  | EAtomNameTypeF (SimpleVarIdentifier ann) (ty ann) ann -- e1: E1
  | EAtomScopeF (ScopeIdentifier ann) ann -- 's
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
