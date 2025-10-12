module Flow.AST.Surface.Syntax where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)

import Flow.AST.Common (AnyVarIdentifier, SimpleVarIdentifier, Scope, AnyTypeIdentifier)
import Flow.AST.Type (Type, EffectRow, Binder, WhereClause)
import Flow.AST.Surface.Pattern (Pattern, ConstructorApp)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Fields (Fields)
import Flow.AST.Surface.Callable (FnDefinitionOf, FnInfixDefinitionOf, OpDefinitionOf, OpInfixDefinitionOf)

-- Statement depends on Expression and vice versa; keep both here

-- Statements and related nodes
data Statement
  = SLet LetDefinition
  | SAssign AssignStatement
  | SReturn Expression
  | SContinue (Maybe SimpleVarIdentifier)
  | SBreak (Maybe SimpleVarIdentifier)
  | SExpression Expression
  deriving (Eq, Ord, Show)

data LetDefinition = LetDefinition
  { lhs :: Pattern
  , lhsType :: Maybe Type
  , mutability :: Bool
  , rhs :: Expression
  }
  deriving (Eq, Ord, Show)

data AssignStatement = AssignStatement
  { lhs :: LHSExpression
  , rhs :: Expression
  }
  deriving (Eq, Ord, Show)

data LHSExpression
  = LHSEWildcard
  | LHSEVar SimpleVarIdentifier
  | LHSEIndex LHSExpression Expression
  | LHSEDotAccess LHSExpression AnyVarIdentifier
  | LHSEUnOp LHSUnOpExpression
  deriving (Eq, Ord, Show)

newtype LHSUnOpExpression
  = LHSUnOpExpressionDeref Expression
  deriving (Eq, Ord, Show)

data BlockExpression = BlockExpression
  { statements :: Vector Statement
  , result :: Maybe Expression
  }
  deriving (Eq, Ord, Show)

-- Expressions

data Expression
  = EWildcard -- _
  | ELiteral Literal -- 0 | true | "str"
  | EOfType Expression Type -- expr : T
  | EVar AnyVarIdentifier -- ident | someModule::ident
  | EIndex Expression Expression -- expr[index]
  | EDotAccess Expression AnyVarIdentifier -- expr.ident
  | EFnCall FnCall -- f(a, b) | f(arg1 = a, arg2 = b) | f<T>(a, b) | f() with { State<S> = e }
  | EUnOp UnOpExpression -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOp BinOpExpression -- a * b | a + b | a ++ b | etc
  | EConstructorAsFn AnyTypeIdentifier -- EnumVariant | Some | Cons
  | EConstructorApp (ConstructorApp Expression) -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | ETuple (NonEmptyVector Expression) -- (a, b, c)
  | EMatch MatchExpression -- match expr { Pattern => expr, ... }
  | EIf IfExpression -- if expr { then_ } else { else_ }
  | ELoop LoopExpression -- loop { ... } | 'label: loop { ... }
  | EWhile WhileExpression -- while expr { ... } | 'label: while expr { ... }
  | EFor ForExpression -- for pattern in iterable { ... }
  | EBlock BlockExpression -- { ... }
  | EHandle HandleExpression -- handle Effect
  | ELambda LambdaExpression -- <A>|a: T, b: T| -> T where { Monoid<T> } { a ++ B }
  deriving (Eq, Ord, Show)

-- Pieces used by both Expr and Stmt

data UnOpExpression = UnOpExpression
  { op :: UnOp
  , operand :: Expression
  }
  deriving (Eq, Ord, Show)

data BinOpExpression = BinOpExpression
  { op :: BinOp
  , left :: Expression
  , right :: Expression
  }
  deriving (Eq, Ord, Show)

-- Small enums for ops (kept minimal to avoid bringing more deps here)
data UnOp
  = UnOpNot
  | UnOpNeg
  | UnOpDeref
  | UnOpTakeRef (Maybe Scope)
  | UnOpTakeMutRef (Maybe Scope)
  deriving (Eq, Ord, Show)

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpMod
  deriving (Eq, Ord, Show)

-- Higher-level expression nodes

data LambdaExpression = LambdaExpression
  { scopes :: Vector Scope
  , binders :: Vector Binder
  , result :: Expression
  , effects :: Maybe Type
  , body :: Expression
  , whereClauses :: Vector WhereClause
  }
  deriving (Eq, Ord, Show)

-- Handle block normalized form

data HandleExpression = HandleExpression
  { in_ :: Maybe EffectRow
  , returning :: Maybe HandleReturning
  , handlers :: NonEmptyVector HandlerSpec
  }
  deriving (Eq, Ord, Show)

data HandlerSpec = HandlerSpec
  { effect :: Type
  , returning :: Maybe HandleReturning
  , body :: NonEmptyVector EffectItemDefinition
  }
  deriving (Eq, Ord, Show)

data HandleReturning = HandleReturning
  { binder :: Binder
  , result :: Type
  }
  deriving (Eq, Ord, Show)

-- Callable references inside expressions are referenced via dedicated types
-- We'll keep these constructors here but their definitions live elsewhere

data FnCall = FnCall
  { callee :: Expression
  , scopeParams :: Maybe (NonEmptyVector Scope)
  , typeParams :: Maybe (NonEmptyVector Type)
  , args :: Vector (Fields Expression)
  , withEffects :: Maybe (Vector WithEffectsItem)
  }
  deriving (Eq, Ord, Show)

data WithEffectsItem = WithEffectsItem
  { lhs :: Either SimpleVarIdentifier Type
  , rhs :: Either SimpleVarIdentifier Type
  }
  deriving (Eq, Ord, Show)

-- Match and control flow

data MatchExpression = MatchExpression
  { value :: Expression
  , arms :: NonEmptyVector MatchArm
  }
  deriving (Eq, Ord, Show)

data MatchArm = MatchArm
  { pattern :: Pattern
  , guard :: Maybe Expression
  , expression :: Expression
  }
  deriving (Eq, Ord, Show)

data IfExpression = IfExpression
  { condition :: Expression
  , then_ :: Expression
  , else_ :: Maybe Expression
  }
  deriving (Eq, Ord, Show)

data LoopExpression = LoopExpression
  { label :: Maybe SimpleVarIdentifier
  , body :: Expression
  }
  deriving (Eq, Ord, Show)

data WhileExpression = WhileExpression
  { label :: Maybe SimpleVarIdentifier
  , condition :: Expression
  , body :: Expression
  }
  deriving (Eq, Ord, Show)

data ForExpression = ForExpression
  { label :: Maybe SimpleVarIdentifier
  , pattern :: Pattern
  , iterable :: Expression
  , body :: Expression
  }
  deriving (Eq, Ord, Show)

-- Backward-compat pattern synonyms (if needed later)

-- Effect handler item definitions, parameterized by callable body type
data EffectItemDefinitionOf body
  = EDefinitionLet LetDefinition
  | EDefinitionFn (FnDefinitionOf body)
  | EDefinitionFnInfix (FnInfixDefinitionOf body)
  | EDefinitionOp (OpDefinitionOf body)
  | EDefinitionOpInfix (OpInfixDefinitionOf body)
  deriving (Eq, Ord, Show)

-- Specialized alias for surface AST, where callable bodies are 'BlockExpression'
type EffectItemDefinition = EffectItemDefinitionOf BlockExpression
