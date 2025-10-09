module Flow.AST.Core.Type where

import "vector" Data.Vector (Vector)
import Flow.AST.Core.Type.Common (Name, ScopeName)
import Flow.AST.Core.Type.Binder (TyBinder, UniverseAnn)
import Flow.AST.Core.Type.Effect (EffectF, EffectRowF)
import Flow.AST.Core.Type.Constraint (ConstraintF)
import Flow.AST.Core.Type.Alias (AliasDecl)

-- | Built-in ground types of the language (Type₀ side).
data Builtin
  = BuiltinUnit
  | BuiltinNever
  | BuiltinBool
  | BuiltinI8
  | BuiltinI16
  | BuiltinI32
  | BuiltinI64
  | BuiltinU8
  | BuiltinU16
  | BuiltinU32
  | BuiltinU64
  | BuiltinF32
  | BuiltinF64
  | BuiltinByte
  | BuiltinByteString
  | BuiltinChar
  | BuiltinString
  deriving (Eq, Ord, Show)

-- | Mutability marker for references.
data RefMutability = RefImm | RefMut
  deriving (Eq, Ord, Show)

-- | Core type AST.
-- This representation lives on the Type-universe side but includes
-- explicit binders, universes for type-level reasoning, effect rows,
-- and saturated 'where' constraints on function types.
data Type
  = TyVar Name                            -- type variable
  | TyBuiltin Builtin                     -- built-in ground type
  | TyCon Name                            -- named type constructor (Vec, Option, ...)
  | TyApp Type (Vector Type)              -- saturated application T<A1,...,An>
  | TyTuple (Vector Type)                 -- (A1, ..., An) : Type
  | TyRef
      { refScope :: ScopeName             -- scope variable name ('s)
      , refMut :: RefMutability           -- & vs & mut
      , refInner :: Type                  -- referenced type
      }
  | TyForall (Vector TyBinder) Type       -- <binders> T
  | TyFn
      { fnArgs :: Vector Type             -- arguments
      , fnEffects :: EffectRowF (EffectF Type) -- effect row @[...]
      , fnResult :: Type                  -- result type
      , fnWhereConstraints :: Vector (ConstraintF Type) -- where { ... } saturated predicates
      , fnWhereAliases :: Vector (AliasDecl Type)       -- where { type ... = ... }
      }
  | TyUniverse UniverseAnn                -- type-level mention of a universe (Typeₙ, Effectₙ, ...)
  deriving (Eq, Ord, Show)
