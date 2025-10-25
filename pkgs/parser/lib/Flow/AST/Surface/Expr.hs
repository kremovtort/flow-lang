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
  AnyTypeIdentifier,
  AnyVarIdentifier,
  ScopeIdentifier,
  SimpleVarIdentifier,
 )
import Flow.AST.Surface.Constraint (BindersWConstraintsF, WhereBlockF)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Syntax (
  CodeBlockF,
  ConstructorAppF,
  ForExpression,
  IfExpression,
  LetDefinitionF,
  LoopExpression,
  MatchExpression,
  WhileExpression,
 )

-- Expressions

data ExpressionF lhsExpr simPat pat ty expr ann
  = EWildcard -- _
  | ELiteral Literal -- 0 | true | "str"
  | EOfType (expr ann) (ty ann) -- expr : T
  | EParens (expr ann) -- (expr)
  | EVar (AnyVarIdentifier ann) -- ident | someModule::ident
  | EIndex (expr ann) (expr ann) -- expr[index]
  | EDotAccess (expr ann) (AnyVarIdentifier ann) -- expr.ident
  | EFnCall (FnCallF ty expr ann) -- f(a, b) | f(arg1 = a, arg2 = b) | f<T>(a, b) | f() with { State<S> = e }
  | EUnOp (UnOpExpression expr ann) -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOp (BinOpExpression expr ann) -- a * b | a + b | a ++ b | etc
  | EConstructor (AnyTypeIdentifier ann) -- EnumVariant | Some | Cons
  | EConstructorApp (ConstructorAppF expr ty ann) -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | ETuple (NonEmptyVector (expr ann)) -- (a, b, c)
  | EMatch (MatchExpression pat expr ann) -- match expr { Pattern => expr, ... }
  | EIf (IfExpression expr ann) -- if expr { then_ } else { else_ }
  | ELoop (LoopExpression expr ann) -- loop { ... } | 'label: loop { ... }
  | EWhile (WhileExpression expr ann) -- while expr { ... } | 'label: while expr { ... }
  | EFor (ForExpression pat expr ann) -- for pattern in iterable { ... }
  | EBlock (CodeBlockF lhsExpr simPat pat ty expr ann) -- { ... }
  | EHandle (HandleExpressionF lhsExpr simPat pat ty expr ann) -- handle Effect
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

data HandleExpressionF lhsExpr simPat pat ty expr ann = HandleExpressionF
  { in_ :: Maybe (ty ann)
  , inAnn :: ann
  , returning :: Maybe (HandleReturningF ty ann)
  , returningAnn :: ann
  , handlers :: NonEmptyVector (HandlerSpecF lhsExpr simPat pat ty expr ann)
  , handlersAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data HandlerSpecF lhsExpr simPat pat ty expr ann = HandlerSpecF
  { effect :: ty ann
  , effectAnn :: ann
  , returning :: Maybe (HandleReturningF ty ann)
  , returningAnn :: ann
  , body :: NonEmptyVector (EffectItemDefinitionF lhsExpr simPat pat ty expr ann)
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

data FnCallF ty expr ann = FnCallF
  { callee :: expr ann
  , calleeAnn :: ann
  , scopeParams :: Maybe (NonEmptyVector (ScopeIdentifier ann))
  , typeParams :: Maybe (NonEmptyVector (ty ann))
  , typeParamsAnn :: ann
  , args :: FnArgsF expr ann
  , argsAnn :: ann
  , withEffects :: Maybe (Vector (WithEffectsItem ty ann))
  , withEffectsAnn :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FnArgsF expr ann
  = FnArgsUnnamedF (Vector (expr ann)) ann
  | FnArgsNamedF (Vector (SimpleVarIdentifier ann, expr ann)) ann
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
data EffectItemDefinitionF lhsExpr simPat pat ty expr ann
  = EDefinitionLetF (LetDefinitionF pat ty expr ann) ann
  | EDefinitionFnF (FnDefinitionF lhsExpr simPat pat ty expr ann) ann
  | EDefinitionFnInfixF (FnInfixDefinitionF lhsExpr simPat pat ty expr ann) ann
  | EDefinitionOpF (OpDefinitionF lhsExpr simPat pat ty expr ann) ann
  | EDefinitionOpInfixF (OpInfixDefinitionF lhsExpr simPat pat ty expr ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
