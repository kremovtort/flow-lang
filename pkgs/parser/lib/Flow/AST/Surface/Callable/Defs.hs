module Flow.AST.Surface.Callable.Defs where

import "base" Prelude hiding (Enum)

-- current package imports
import Flow.AST.Surface.Syntax (BlockExpression)
import Flow.AST.Surface.Callable (Callable, CallKind (KFn, KOp), Fixity (Plain, Infix), Phase (Def))

-- | Type synonyms for callable definitions specialized to 'BlockExpression'
type FnDefinition        = Callable 'KFn 'Plain 'Def BlockExpression
type FnInfixDefinition   = Callable 'KFn 'Infix 'Def BlockExpression
type OpDefinition        = Callable 'KOp 'Plain 'Def BlockExpression
type OpInfixDefinition   = Callable 'KOp 'Infix 'Def BlockExpression


