module Flow.AST.Surface.Expr where

import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Common (AnyTypeIdentifier, AnyVarIdentifier, ScopeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Callable
import Flow.AST.Surface.Constraint (BinderF, WhereClauseF)
import Flow.AST.Surface.Fields (Fields)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Pattern (ConstructorApp)
import Flow.AST.Surface.Syntax (CodeBlockF, LetDefinitionF)

-- Expressions

data ExpressionF lhsExpr pat ty expr ann
  = EWildcard ann -- _
  | ELiteral Literal ann -- 0 | true | "str"
  | EOfType (expr ann) (ty ann) ann -- expr : T
  | EVar (AnyVarIdentifier ann) -- ident | someModule::ident
  | EIndex (expr ann) (expr ann) ann -- expr[index]
  | EDotAccess (expr ann) (AnyVarIdentifier ann) ann -- expr.ident
  | EFnCall (FnCallF ty expr ann) -- f(a, b) | f(arg1 = a, arg2 = b) | f<T>(a, b) | f() with { State<S> = e }
  | EUnOp (UnOpExpression expr ann) -- -a | !a | *a | &a | &mut a | &'s mut a
  | EBinOp (BinOpExpression expr ann) -- a * b | a + b | a ++ b | etc
  | EConstructorAsFn (AnyTypeIdentifier ann) -- EnumVariant | Some | Cons
  | EConstructorApp (ConstructorApp expr ann) -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | ETuple (NonEmptyVector (expr ann)) ann -- (a, b, c)
  | EMatch (MatchExpression pat expr ann) -- match expr { Pattern => expr, ... }
  | EIf (IfExpression expr ann) -- if expr { then_ } else { else_ }
  | ELoop (LoopExpression expr ann) -- loop { ... } | 'label: loop { ... }
  | EWhile (WhileExpression expr ann) -- while expr { ... } | 'label: while expr { ... }
  | EFor (ForExpression pat expr ann) -- for pattern in iterable { ... }
  | EBlock (CodeBlockF lhsExpr pat ty expr ann) -- { ... }
  | EHandle (HandleExpressionF lhsExpr pat ty expr ann) -- handle Effect
  | ELambda (LambdaExpressionF ty expr ann) -- <A>|a: T, b: T| -> T where { Monoid<T> } { a ++ B }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Pieces used by both Expr and Stmt

data UnOpExpression expr ann = UnOpExpression
  { op :: UnOp ann
  , operand :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data BinOpExpression expr ann = BinOpExpression
  { op :: BinOp
  , opAnn :: ann
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

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpMod
  deriving (Eq, Ord, Show)

-- Higher-level expression nodes

data LambdaExpressionF ty expr ann = LambdaExpressionF
  { scopes :: Vector (ScopeIdentifier ann)
  , binders :: Vector (BinderF ty ann)
  , result :: ty ann
  , effects :: Maybe (ty ann)
  , body :: expr ann
  , whereClauses :: Vector (WhereClauseF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Handle block normalized form

data HandleExpressionF lhsExpr pat ty expr ann = HandleExpressionF
  { in_ :: Maybe (ty ann)
  , returning :: Maybe (HandleReturningF ty ann)
  , handlers :: NonEmptyVector (HandlerSpecF lhsExpr pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data HandlerSpecF lhsExpr pat ty expr ann = HandlerSpecF
  { effect :: ty ann
  , returning :: Maybe (HandleReturningF ty ann)
  , body :: NonEmptyVector (EffectItemDefinitionF lhsExpr pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data HandleReturningF ty ann = HandleReturningF
  { binder :: ReturningBinderF ty ann
  , result :: ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ReturningBinderF ty ann = ReturningBinderF
  { name :: SimpleVarIdentifier ann
  , result :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Callable references inside expressions are referenced via dedicated types
-- We'll keep these constructors here but their definitions live elsewhere

data FnCallF ty expr ann = FnCallF
  { callee :: expr ann
  , scopeParams :: Maybe (NonEmptyVector (ScopeIdentifier ann))
  , typeParams :: Maybe (NonEmptyVector (ty ann))
  , args :: Vector (Fields expr ann)
  , withEffects :: Maybe (Vector (WithEffectsItem ty ann))
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WithEffectsItem ty ann = WithEffectsItem
  { lhs :: Either (SimpleVarIdentifier ann) (ty ann)
  , rhs :: Either (SimpleVarIdentifier ann) (ty ann)
  }
  deriving (Eq, Ord, Show)

instance (Functor ty) => Functor (WithEffectsItem ty) where
  fmap f (WithEffectsItem lhs rhs) =
    WithEffectsItem
      { lhs = case lhs of
          Left a -> Left (fmap f a)
          Right a -> Right (fmap f a)
      , rhs = case rhs of
          Left a -> Left (fmap f a)
          Right a -> Right (fmap f a)
      }

instance (Foldable ty) => Foldable (WithEffectsItem ty) where
  foldMap f (WithEffectsItem lhs rhs) = lhsFolded <> rhsFolded
    where
      lhsFolded = case lhs of
        Left a -> foldMap f a
        Right a -> foldMap f a
      rhsFolded = case rhs of
        Left a -> foldMap f a
        Right a -> foldMap f a

instance (Traversable ty) => Traversable (WithEffectsItem ty) where
  traverse f (WithEffectsItem lhs rhs) = liftA2 WithEffectsItem lhsTraversed rhsTraversed
    where
      lhsTraversed = case lhs of
        Left a -> Left <$> traverse f a
        Right a -> Right <$> traverse f a
      rhsTraversed = case rhs of
        Left a -> Left <$> traverse f a
        Right a -> Right <$> traverse f a


-- Match and control flow

data MatchExpression pat expr ann = MatchExpression
  { value :: expr ann
  , arms :: NonEmptyVector (MatchArm pat expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data MatchArm pat expr ann = MatchArm
  { pattern :: pat ann
  , guard :: Maybe (expr ann)
  , expression :: expr ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data IfExpression expr ann = IfExpression
  { condition :: expr ann
  , then_ :: expr ann
  , else_ :: Maybe (expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LoopExpression expr ann = LoopExpression
  { label :: Maybe (SimpleVarIdentifier ann)
  , body :: expr ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WhileExpression expr ann = WhileExpression
  { label :: Maybe (SimpleVarIdentifier ann)
  , condition :: expr ann
  , body :: expr ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ForExpression pat expr ann = ForExpression
  { label :: Maybe (SimpleVarIdentifier ann)
  , pattern :: pat ann
  , iterable :: expr ann
  , body :: expr ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Effect handler item definitions, parameterized by callable body type
data EffectItemDefinitionF lhsExpr pat ty expr ann
  = EDefinitionLetF (LetDefinitionF pat ty expr ann)
  | EDefinitionFnF (FnDefinitionF lhsExpr pat ty expr ann)
  | EDefinitionFnInfixF (FnInfixDefinitionF lhsExpr pat ty expr ann)
  | EDefinitionOpF (OpDefinitionF lhsExpr pat ty expr ann)
  | EDefinitionOpInfixF (OpInfixDefinitionF lhsExpr pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
