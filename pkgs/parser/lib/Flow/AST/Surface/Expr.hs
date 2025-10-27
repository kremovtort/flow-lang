{-# LANGUAGE RecordWildCards #-}

module Flow.AST.Surface.Expr where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable (
  FnDefinitionF,
  FnInfixDefinitionF,
  OpDefinitionF,
  OpInfixDefinitionF,
 )
import Flow.AST.Surface.Common (
  ScopeIdentifier,
  SimpleVarIdentifier,
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
  ForExpressionF,
  IfExpressionF,
  LetDefinitionF,
  LoopExpressionF,
  MatchExpressionF,
  WhileExpressionF,
 )

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
  | EUnOp (UnOpExpression expr ann) -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOp (BinOpExpression expr ann) -- a * b | a + b | a ++ b | etc
  | EAppF (AppF ty expr ann) -- f(a, b, c) | f(a, b, c) with {}
  | ETuple (NonEmptyVector (expr ann)) -- (a, b, c)
  | EMatch (MatchExpressionF pat expr ann) -- match expr { Pattern => expr, ... }
  | EIf (IfExpressionF stmt pat expr ann) -- if expr { then_ } else { else_ }
  | ELoop (LoopExpressionF stmt expr ann) -- loop { ... } | 'label: loop { ... }
  | EWhile (WhileExpressionF stmt pat expr ann) -- while expr { ... } | 'label: while expr { ... }
  | EFor (ForExpressionF pat expr ann) -- for pattern in iterable { ... }
  | EBlock (CodeBlockF stmt expr ann) -- { ... }
  | EHandle (HandleExpressionF stmt simPat ty expr ann) -- handle Effect
  | ELambda (LambdaExpressionF ty expr ann) -- <A>|a: T, b: T| -> T where { Monoid<T> } { a ++ B }
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

data LambdaExpressionF ty expr ann = LambdaExpressionF
  { typeParams :: Maybe (BindersWConstraintsF ty ann)
  , effects :: Maybe (ty ann)
  , effectsAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , body :: expr ann
  , bodyAnn :: ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Handle block normalized form

data HandleExpressionF stmt simPat ty expr ann = HandleExpressionF
  { in_ :: Maybe (ty ann)
  , inAnn :: ann
  , returning :: Maybe (HandleReturningF ty ann)
  , returningAnn :: ann
  , handlers :: NonEmptyVector (HandlerSpecF stmt simPat ty expr ann)
  , handlersAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandlerSpecF stmt simPat ty expr ann = HandlerSpecF
  { effect :: ty ann
  , effectAnn :: ann
  , returning :: Maybe (HandleReturningF ty ann)
  , returningAnn :: ann
  , body :: NonEmptyVector (EffectItemDefinitionF stmt simPat ty expr ann)
  , bodyAnn :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandleReturningF ty ann = HandleReturningF
  { binder :: ReturningBinderF ty ann
  , binderAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ReturningBinderF ty ann = ReturningBinderF
  { name :: SimpleVarIdentifier ann
  , nameAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Callable references inside expressions are referenced via dedicated types
-- We'll keep these constructors here but their definitions live elsewhere

data AppF ty expr ann = AppF
  { callee :: expr ann
  , typeParams :: BindersAppF ty ann
  , args :: AppArgsF expr ann
  , argsAnn :: ann
  , withEffects :: Maybe (Vector (WithEffectsItem ty ann), ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AppArgsF expr ann
  = AppArgsUnnamedF (Vector (expr ann))
  | AppArgsNamedF (Vector (ArgNamedF expr ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ArgNamedF expr ann = FieldNamedF
  { name :: SimpleVarIdentifier ann
  , optional :: Maybe ann
  , value :: Maybe (expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithEffectsItem ty ann = WithEffectsItem
  { lhs :: Either (SimpleVarIdentifier ann) (ty ann)
  , rhs :: Either (SimpleVarIdentifier ann) (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Generic, ToExpr)

instance (Functor ty) => Functor (WithEffectsItem ty) where
  fmap f (WithEffectsItem{..}) =
    WithEffectsItem
      { lhs = case lhs of
          Left a -> Left (fmap f a)
          Right a -> Right (fmap f a)
      , rhs = case rhs of
          Left a -> Left (fmap f a)
          Right a -> Right (fmap f a)
      , ann = f ann
      }

instance (Foldable ty) => Foldable (WithEffectsItem ty) where
  foldMap f (WithEffectsItem{lhs, rhs, ann}) = lhsFolded <> rhsFolded <> f ann
   where
    lhsFolded = case lhs of
      Left a -> foldMap f a
      Right a -> foldMap f a
    rhsFolded = case rhs of
      Left a -> foldMap f a
      Right a -> foldMap f a

instance (Traversable ty) => Traversable (WithEffectsItem ty) where
  traverse f (WithEffectsItem{..}) =
    WithEffectsItem <$> lhsTraversed <*> rhsTraversed <*> f ann
   where
    lhsTraversed = case lhs of
      Left a -> Left <$> traverse f a
      Right a -> Right <$> traverse f a
    rhsTraversed = case rhs of
      Left a -> Left <$> traverse f a
      Right a -> Right <$> traverse f a

-- Effect handler item definitions, parameterized by callable body type
data EffectItemDefinitionF stmt simPat ty expr ann
  = EDefinitionLetF (LetDefinitionF simPat ty expr ann) ann
  | EDefinitionFnF (FnDefinitionF stmt ty expr ann) ann
  | EDefinitionFnInfixF (FnInfixDefinitionF stmt ty expr ann) ann
  | EDefinitionOpF (OpDefinitionF stmt ty expr ann) ann
  | EDefinitionOpInfixF (OpInfixDefinitionF stmt ty expr ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
