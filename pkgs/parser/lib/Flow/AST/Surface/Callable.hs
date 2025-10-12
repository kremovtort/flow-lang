{-# LANGUAGE DataKinds #-}

module Flow.AST.Surface.Callable where

import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Common (ScopeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (BinderF, WhereClauseF)
import Flow.AST.Surface.Syntax (CodeBlockF, UnitF)

data CallKind = KFn | KOp
data Fixity = Plain | Infix

-- | Receiver header for infix calls
data ReceiverHeader ty ann = ReceiverHeader
  { calleeScopeParams :: Vector (ScopeIdentifier ann)
  , calleeTypeParams :: Vector (BinderF ty ann)
  , calleeName :: SimpleVarIdentifier ann
  , calleeType :: ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Common header for all callable entities
data CallableHeader ty reciever ann = CallableHeader
  { receiver :: reciever ann
  , name :: SimpleVarIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderF ty ann)
  , args :: Vector (SimpleVarIdentifier ann, ty ann)
  , effects :: Maybe (ty ann)
  , result :: Maybe (ty ann)
  , whereClauses :: Vector (WhereClauseF ty ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data RecieverF (fixity :: Fixity) ty ann where
  RecieverFPlain :: () -> RecieverF Plain ty ann
  RecieverFInfix :: ReceiverHeader ty ann -> RecieverF Infix ty ann

deriving instance (Show ann, Show (ty ann)) => Show (RecieverF f ty ann)
deriving instance (Eq ann, Eq (ty ann)) => Eq (RecieverF f ty ann)
deriving instance (Ord ann, Ord (ty ann)) => Ord (RecieverF f ty ann)
deriving instance (Functor ty) => Functor (RecieverF fixity ty)
deriving instance (Foldable ty) => Foldable (RecieverF fixity ty)
deriving instance (Traversable ty) => Traversable (RecieverF fixity ty)

data
  CallableF
    (kind :: CallKind)
    (fixity :: Fixity)
    ty
    body
    ann
  = CallableF
  { header :: CallableHeader ty (RecieverF fixity ty) ann
  , body :: body ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type FnDeclarationF ty ann =
  CallableF
    'KFn
    'Plain
    ty
    UnitF
    ann

type FnInfixDeclarationF ty ann =
  CallableF
    'KFn
    'Infix
    ty
    UnitF
    ann

type OpDeclarationF ty ann =
  CallableF
    'KOp
    'Plain
    ty
    UnitF
    ann

type OpInfixDeclarationF ty ann =
  CallableF
    'KOp
    'Infix
    ty
    UnitF
    ann

type FnDefinitionF lhsExpr pat ty expr ann =
  CallableF
    'KFn
    'Plain
    ty
    (CodeBlockF lhsExpr pat ty expr)
    ann

type FnInfixDefinitionF lhsExpr pat ty expr ann =
  CallableF
    'KFn
    'Infix
    ty
    (CodeBlockF lhsExpr pat ty expr)
    ann

type OpDefinitionF lhsExpr pat ty expr ann =
  CallableF
    'KOp
    'Plain
    ty
    (CodeBlockF lhsExpr pat ty expr)
    ann

type OpInfixDefinitionF lhsExpr pat ty expr ann =
  CallableF
    'KOp
    'Infix
    ty
    (CodeBlockF lhsExpr pat ty expr)
    ann
