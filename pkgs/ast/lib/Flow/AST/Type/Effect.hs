module Flow.AST.Type.Effect where

import "vector" Data.Vector (Vector)

import Flow.AST.Type.Common (Name, ScopeName)

-- | Primitive effect atom, parameterized by the type node.
data EffectF ty
  = EffName Name (Vector ty)   -- IO, State<S>, FileIO, Error<E>
  | EffScope ScopeName         -- Scope<'s>
  deriving (Eq, Ord, Show)

-- | Effect row with optional tail variable (open row)
data EffectRowF eff = EffectRow
  { effects :: Vector eff
  , tailVar :: Maybe Name         -- ..R
  }
  deriving (Eq, Ord, Show)


