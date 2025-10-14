{-# LANGUAGE RecordWildCards #-}

module Flow.AST.Surface.Expr where

import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable
import Flow.AST.Surface.Common (AnyTypeIdentifier, AnyVarIdentifier, ScopeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (BinderF, WhereClauseF)
import Flow.AST.Surface.Fields (Fields)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Pattern (ConstructorApp)
import Flow.AST.Surface.Syntax (CodeBlockF, ForExpression, IfExpression, LetDefinitionF, LoopExpression, MatchExpression, WhileExpression)

-- Expressions

data ExpressionF lhsExpr pat ty expr ann
  = EWildcard ann -- _
  | ELiteral (Literal ann) ann -- 0 | true | "str"
  | EOfType (expr ann) (ty ann) ann -- expr : T
  | EParens (expr ann) ann -- (expr)
  | EVar (AnyVarIdentifier ann) ann -- ident | someModule::ident
  | EIndex (expr ann) (expr ann) ann -- expr[index]
  | EDotAccess (expr ann) (AnyVarIdentifier ann) ann -- expr.ident
  | EFnCall (FnCallF ty expr ann) ann -- f(a, b) | f(arg1 = a, arg2 = b) | f<T>(a, b) | f() with { State<S> = e }
  | EUnOp (UnOpExpression expr ann) ann -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOp (BinOpExpression expr ann) ann -- a * b | a + b | a ++ b | etc
  | EConstructorAsFn (AnyTypeIdentifier ann) ann -- EnumVariant | Some | Cons
  | EConstructorApp (ConstructorApp expr ann) ann -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | ETuple (NonEmptyVector (expr ann)) ann -- (a, b, c)
  | EMatch (MatchExpression pat expr ann) ann -- match expr { Pattern => expr, ... }
  | EIf (IfExpression expr ann) ann -- if expr { then_ } else { else_ }
  | ELoop (LoopExpression expr ann) ann -- loop { ... } | 'label: loop { ... }
  | EWhile (WhileExpression expr ann) ann -- while expr { ... } | 'label: while expr { ... }
  | EFor (ForExpression pat expr ann) ann -- for pattern in iterable { ... }
  | EBlock (CodeBlockF lhsExpr pat ty expr ann) ann -- { ... }
  | EHandle (HandleExpressionF lhsExpr pat ty expr ann) -- handle Effect
  | ELambda (LambdaExpressionF ty expr ann) ann -- <A>|a: T, b: T| -> T where { Monoid<T> } { a ++ B }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Pieces used by both Expr and Stmt

data UnOpExpression expr ann = UnOpExpression
  { op :: UnOp ann
  , operand :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BinOpExpression expr ann = BinOpExpression
  { op :: BinOp ann
  , left :: expr ann
  , right :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Small enums for ops (kept minimal to avoid bringing more deps here)
data UnOp ann
  = UnOpNot ann
  | UnOpNeg ann
  | UnOpDeref ann
  | UnOpTakeRef (Maybe (ScopeIdentifier ann)) ann
  | UnOpTakeMutRef (Maybe (ScopeIdentifier ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Higher-level expression nodes

data LambdaExpressionF ty expr ann = LambdaExpressionF
  { scopes :: Vector (ScopeIdentifier ann)
  , binders :: Vector (BinderF ty ann)
  , typeParamsAnn :: ann
  , effects :: Maybe (ty ann)
  , effectsAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , body :: expr ann
  , bodyAnn :: ann
  , whereClauses :: Vector (WhereClauseF ty ann)
  , whereClausesAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Handle block normalized form

data HandleExpressionF lhsExpr pat ty expr ann = HandleExpressionF
  { in_ :: Maybe (ty ann)
  , inAnn :: ann
  , returning :: Maybe (HandleReturningF ty ann)
  , returningAnn :: ann
  , handlers :: NonEmptyVector (HandlerSpecF lhsExpr pat ty expr ann)
  , handlersAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data HandlerSpecF lhsExpr pat ty expr ann = HandlerSpecF
  { effect :: ty ann
  , effectAnn :: ann
  , returning :: Maybe (HandleReturningF ty ann)
  , returningAnn :: ann
  , body :: NonEmptyVector (EffectItemDefinitionF lhsExpr pat ty expr ann)
  , bodyAnn :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data HandleReturningF ty ann = HandleReturningF
  { binder :: ReturningBinderF ty ann
  , binderAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ReturningBinderF ty ann = ReturningBinderF
  { name :: SimpleVarIdentifier ann
  , nameAnn :: ann
  , result :: ty ann
  , resultAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Callable references inside expressions are referenced via dedicated types
-- We'll keep these constructors here but their definitions live elsewhere

data FnCallF ty expr ann = FnCallF
  { callee :: expr ann
  , calleeAnn :: ann
  , scopeParams :: Maybe (NonEmptyVector (ScopeIdentifier ann))
  , typeParams :: Maybe (NonEmptyVector (ty ann))
  , typeParamsAnn :: ann
  , args :: Vector (Fields expr ann)
  , argsAnn :: ann
  , withEffects :: Maybe (Vector (WithEffectsItem ty ann))
  , withEffectsAnn :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WithEffectsItem ty ann = WithEffectsItem
  { lhs :: Either (SimpleVarIdentifier ann) (ty ann)
  , lhsAnn :: ann
  , rhs :: Either (SimpleVarIdentifier ann) (ty ann)
  , rhsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show)

instance (Functor ty) => Functor (WithEffectsItem ty) where
  fmap f (WithEffectsItem{..}) =
    WithEffectsItem
      { lhs = case lhs of
          Left a -> Left (fmap f a)
          Right a -> Right (fmap f a)
      , lhsAnn = f lhsAnn
      , rhs = case rhs of
          Left a -> Left (fmap f a)
          Right a -> Right (fmap f a)
      , rhsAnn = f rhsAnn
      , ann = f ann
      }

instance (Foldable ty) => Foldable (WithEffectsItem ty) where
  foldMap f (WithEffectsItem{..}) = lhsFolded <> f lhsAnn <> rhsFolded <> f rhsAnn <> f ann
   where
    lhsFolded = case lhs of
      Left a -> foldMap f a
      Right a -> foldMap f a
    rhsFolded = case rhs of
      Left a -> foldMap f a
      Right a -> foldMap f a

instance (Traversable ty) => Traversable (WithEffectsItem ty) where
  traverse f (WithEffectsItem{..}) =
    WithEffectsItem <$> lhsTraversed <*> f lhsAnn <*> rhsTraversed <*> f rhsAnn <*> f ann
   where
    lhsTraversed = case lhs of
      Left a -> Left <$> traverse f a
      Right a -> Right <$> traverse f a
    rhsTraversed = case rhs of
      Left a -> Left <$> traverse f a
      Right a -> Right <$> traverse f a

-- Effect handler item definitions, parameterized by callable body type
data EffectItemDefinitionF lhsExpr pat ty expr ann
  = EDefinitionLetF (LetDefinitionF pat ty expr ann) ann
  | EDefinitionFnF (FnDefinitionF lhsExpr pat ty expr ann) ann
  | EDefinitionFnInfixF (FnInfixDefinitionF lhsExpr pat ty expr ann) ann
  | EDefinitionOpF (OpDefinitionF lhsExpr pat ty expr ann) ann
  | EDefinitionOpInfixF (OpInfixDefinitionF lhsExpr pat ty expr ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
