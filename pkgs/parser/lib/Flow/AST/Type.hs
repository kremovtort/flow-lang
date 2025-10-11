module Flow.AST.Type where

import "vector" Data.Vector (Vector)

import Flow.AST.Common (Scope, SimpleTypeIdentifier, AnyTypeIdentifier, SimpleVarIdentifier)

data TypeDefinition = TypeDefinition
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector BinderWoConstraint
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data Type
  = TyBuiltin Builtin
  | TyIdentifier AnyTypeIdentifier -- MyType
  | TyApp App -- Option<A>
  | TyTuple (Vector Type) -- (A, B, C)
  | TyRef Ref -- &'s T | &'s mut T | &T | &mut T
  | TyForall (Forall Type) -- <A :< Monoid> fn(List<A>) -> A
  | TyFn Fn -- fn(List<A>) -> A
  | TyEffectful Effectful -- @A | @[IO] A
  | TyEffectRow EffectRow -- [IO, State<S>] | ['s, IO], | [IO, ..R, ..S, ..] | etc
  deriving (Eq, Ord, Show)

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

data App = App -- Option<A>
  { head :: Type
  , args :: Vector Type
  }
  deriving (Eq, Ord, Show)

data Ref = Ref -- &'s T | &'s mut T | &T | &mut T
  { scope :: Maybe Scope
  , mutability :: Bool
  , inner :: Type
  }
  deriving (Eq, Ord, Show)

data Forall t = Forall -- <A :< Monoid> fn(List<A>) -> A
  { scopes :: Vector Scope
  , binders :: Vector Binder
  , result :: t
  , whereClauses :: Vector WhereClause
  }
  deriving (Eq, Ord, Show)

data Fn = Fn -- fn(List<A>) -> A
  { args :: Vector Type
  , result :: Type
  }
  deriving (Eq, Ord, Show)

data Effectful = Effectful -- @R | @[IO] A
  { effects :: Maybe EffectRow
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data WhereClause -- type X = Y | Functor<A> | etc
  = WhereConstraint Type
  | WhereAlias TypeDefinition
  deriving (Eq, Ord, Show)

data Binder = Binder -- A | A :< Monoid
  { name :: SimpleTypeIdentifier
  , type_ :: Type
  , constraint :: Vector AnyTypeIdentifier
  }
  deriving (Eq, Ord, Show)

data BinderWoConstraint = BinderWoConstraint
  { name :: SimpleTypeIdentifier
  , typeType :: Maybe Type
  }
  deriving (Eq, Ord, Show)

data EffectRow = EffectRow
  { effects :: Vector EffectAtom
  , rows :: Vector AnyTypeIdentifier
  , open :: Bool
  }
  deriving (Eq, Ord, Show)

data EffectAtom
  = EAtomName SimpleVarIdentifier -- e1
  | EAtomType Type -- E1
  | EAtomNameType SimpleVarIdentifier Type -- e1: E1
  | EAtomScope Scope -- 's
  deriving (Eq, Ord, Show)
