module Flow.AST.Surface.Expr where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable (
  OpDefinitionF,
  OpInfixDefinitionF,
 )
import Flow.AST.Surface.Common (
  ScopeIdentifier,
  SimpleVarIdentifier, SimpleTypeIdentifier,
 )
import Flow.AST.Surface.Constraint (
  AnyTypeIdentifier,
  AnyVarIdentifier,
  BindersAppF,
  BindersWConstraintsF,
  WhereBlockF,
 )
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Syntax (
  CodeBlockF,
  IfExpressionF,
  LetDefinitionF,
  LoopExpressionF,
  MatchExpressionF,
  WithF,
  WithStatementF,
 )
import Flow.AST.Surface.Use (UseClause)
import Flow.AST.Surface.Type (EffectRowF, FnEffectsResultF)

-- Expressions

data ExpressionF stmt simPat pat ty expr ann
  = EWildcard -- _
  | ELiteral Literal -- 0 | true | "str"
  | EOfType (expr ann) (ty ann) -- expr : T
  | EParens (expr ann) -- (expr)
  | EVar (AnyVarIdentifier ty ann) -- ident | someModule::ident
  | EConstructor (AnyTypeIdentifier ty ann) -- EnumVariant | Some | Cons
  | EIndex (expr ann) (expr ann) -- expr[index]
  | EDotAccess (expr ann) (AnyVarIdentifier ty ann) -- expr.ident
  | EUnOpF (UnOpExpression expr ann) -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOpF (BinOpExpression expr ann) -- a * b | a + b | a ++ b | etc
  | EAppF (AppF ty expr ann) -- f(a, b, c) | f(a, b, c) with {}
  | EWithF (WithF stmt ty expr ann) -- with { let a = b; c = d } in { ... }
  | ETupleF (expr ann) (NonEmptyVector (expr ann)) -- (a, b, c)
  | EMatchF (MatchExpressionF pat expr ann) -- match expr { Pattern => expr, ... }
  | EIfF (IfExpressionF stmt pat expr ann) -- if expr { then_ } else { else_ }
  | ELoopF (LoopExpressionF stmt expr ann) -- loop { ... } | 'label: loop { ... }
  | EBlockF (CodeBlockF stmt expr ann) -- { ... }
  | EHandleF (HandleExpressionF stmt simPat ty expr ann) -- handle Effect
  | ELambdaF (LambdaF stmt ty expr ann) -- <A>|a: T, b: T| -> T where { Monoid<T> } { a ++ B }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Pieces used by both Expr and Stmt

data UnOpExpression expr ann = UnOpExpression
  { op :: UnOp ann
  , operand :: expr ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BinOpExpression expr ann = BinOpExpression
  { op :: BinOp ann
  , left :: expr ann
  , right :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Small enums for ops (kept minimal to avoid bringing more deps here)
data UnOp ann
  = UnOpNot ann
  | UnOpNeg ann
  | UnOpDeref ann
  | UnOpTakeRef (Maybe (ScopeIdentifier ann)) ann
  | UnOpTakeMutRef (Maybe (ScopeIdentifier ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BinOp ann
  = BinOpAdd ann
  | BinOpSub ann
  | BinOpMul ann
  | BinOpDiv ann
  | BinOpMod ann
  | BinOpAnd ann
  | BinOpOr ann
  | BinOpLessThan ann
  | BinOpLessThanOrEqual ann
  | BinOpGreaterThan ann
  | BinOpGreaterThanOrEqual ann
  | BinOpEqual ann
  | BinOpNotEqual ann
  | BinOpConcat ann
  | BinOpBitwiseAnd ann
  | BinOpBitwiseOr ann
  | BinOpBitwiseShiftLeft ann
  | BinOpBitwiseShiftRight ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Higher-level expression nodes

data LambdaF stmt ty expr ann
  = LamShortF (LambdaShortF ty expr ann)
  | LamFullF (LambdaFullF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaShortF ty expr ann = LambdaShortF
  { args :: Vector (LambdaArgF ty ann)
  , body :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaFullF stmt ty expr ann = LambdaFullF
  { typeParams :: Maybe (BindersWConstraintsF ty ann)
  , args :: Vector (LambdaArgF ty ann)
  , effectsResult :: Maybe (FnEffectsResultF ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LambdaArgF ty ann = LambdaArgF
  { mut :: Maybe ann
  , name :: SimpleVarIdentifier ann
  , type_ :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleExpressionF stmt simPat ty expr ann = HandleExpressionF
  { effects :: NonEmptyVector (ty ann)
  , in_ :: Maybe (ty ann)
  , returning :: Maybe (HandleReturningF ty ann)
  , body :: HandleBodyF stmt simPat ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleReturningF ty ann = HandleReturningF
  { binder :: SimpleTypeIdentifier ann
  , result :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleBodyF stmt simPat ty expr ann = HandleBodyF
  { uses :: Vector (UseClause ann)
  , items :: NonEmptyVector (EffectItemDefinitionF stmt simPat ty expr ann, ann)
  , returning :: Maybe (HandleReturningBlockF stmt ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Effect handler item definitions, parameterized by callable body type
data EffectItemDefinitionF stmt simPat ty expr ann
  = EDefinitionLetF (LetDefinitionF simPat ty expr ann)
  | EDefinitionOpF (OpDefinitionF stmt ty expr ann)
  | EDefinitionOpInfixF (OpInfixDefinitionF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleReturningBlockF stmt ty expr ann = HandleReturningBlockF
  { arg :: SimpleVarIdentifier ann
  , argType :: SimpleTypeIdentifier ann
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Callable references inside expressions are referenced via dedicated types
-- We'll keep these constructors here but their definitions live elsewhere

data AppF ty expr ann = AppF
  { callee :: expr ann
  , typeParams :: Maybe (BindersAppF ty ann)
  , args :: AppArgsF expr ann
  , with :: Maybe (NonEmptyVector (WithStatementF ty expr ann))
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AppArgsF expr ann
  = AppArgsUnnamedF (Vector (expr ann))
  | AppArgsNamedF (Vector (ArgNamedF expr ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ArgNamedF expr ann = ArgNamedF
  { name :: SimpleVarIdentifier ann
  , value :: Maybe (expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
