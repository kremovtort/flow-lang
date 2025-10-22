{-# LANGUAGE DataKinds #-}

module Flow.AST.Surface.Callable where

import "base" Prelude hiding (Enum)

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (BindersWConstraintsF, WhereBlockF)
import Flow.AST.Surface.Syntax (CodeBlockF, UnitF)

data CallKind = KFn | KOp
  deriving (Eq, Ord, Show, Generic, ToExpr)

data Fixity = Plain | Infix
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- | Receiver header for infix calls
data ReceiverHeader ty ann = ReceiverHeader
  { typeParams :: Maybe (BindersWConstraintsF ty ann)
  , name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- | Common header for all callable entities
data CallableHeader ty reciever ann = CallableHeader
  { receiver :: reciever ann
  , name :: SimpleVarIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , argsRequired :: Vector (ArgF ty ann)
  , argsRequiredAnn :: ann
  , argsOptional :: Vector (ArgF ty ann)
  , argsOptionalAnn :: ann
  , effects :: Maybe (ty ann, ann)
  , result :: Maybe (ty ann, ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ArgF ty ann = ArgF
  { mut :: Maybe ann
  , name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data
  CallableF
    (kind :: CallKind)
    reciever
    ty
    body
    ann
  = CallableF
  { header :: CallableHeader ty reciever ann
  , body :: body ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type FnDeclarationF ty ann =
  CallableF
    'KFn
    UnitF
    ty
    UnitF
    ann

type FnInfixDeclarationF ty ann =
  CallableF
    'KFn
    (ReceiverHeader ty)
    ty
    UnitF
    ann

type OpDeclarationF ty ann =
  CallableF
    'KOp
    UnitF
    ty
    UnitF
    ann

type OpInfixDeclarationF ty ann =
  CallableF
    'KOp
    (ReceiverHeader ty)
    ty
    UnitF
    ann

type FnDefinitionF lhsExpr simPat pat ty expr ann =
  CallableF
    'KFn
    UnitF
    ty
    (CodeBlockF lhsExpr simPat pat ty expr)
    ann

type FnInfixDefinitionF lhsExpr simPat pat ty expr ann =
  CallableF
    'KFn
    (ReceiverHeader ty)
    ty
    (CodeBlockF lhsExpr simPat pat ty expr)
    ann

type OpDefinitionF lhsExpr simPat pat ty expr ann =
  CallableF
    'KOp
    UnitF
    ty
    (CodeBlockF lhsExpr simPat pat ty expr)
    ann

type OpInfixDefinitionF lhsExpr simPat pat ty expr ann =
  CallableF
    'KOp
    (ReceiverHeader ty)
    ty
    (CodeBlockF lhsExpr simPat pat ty expr)
    ann
