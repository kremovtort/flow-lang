{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Flow.AST.Surface.Callable where

import "base" Prelude hiding (Enum)

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (
  AnyVarIdentifier,
  BindersWoConstraintsF,
  WhereBlockF,
 )
import Flow.AST.Surface.Syntax (CodeBlockF, UnitF)
import Flow.AST.Surface.Type (FnEffectsResultF)

data CallKind = KFn | KOp
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- | Receiver header for infix calls
data ReceiverHeaderF ty ann = ReceiverHeaderF
  { typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- | Common header for all callable entities
data CallableHeader reciever name ty ann = CallableHeader
  { receiver :: reciever ann
  , name :: name ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , args :: Vector (ArgF ty ann)
  , effectsResult :: Maybe (FnEffectsResultF ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , ann :: ann
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
    name
    body
    ty
    ann
  = CallableF
  { header :: CallableHeader reciever name ty ann
  , body :: body ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

type FnDeclarationF =
  CallableF
    'KFn
    UnitF
    SimpleVarIdentifier
    UnitF

type FnInfixDeclarationF ty =
  CallableF
    'KFn
    (ReceiverHeaderF ty)
    SimpleVarIdentifier
    UnitF
    ty

type OpDeclarationF =
  CallableF
    'KOp
    UnitF
    SimpleVarIdentifier
    UnitF

type OpInfixDeclarationF ty =
  CallableF
    'KOp
    (ReceiverHeaderF ty)
    SimpleVarIdentifier
    UnitF
    ty

type FnDefinitionF stmt ty expr =
  CallableF
    'KFn
    UnitF
    SimpleVarIdentifier
    (CodeBlockF stmt expr)
    ty

type FnInfixDefinitionF stmt ty expr =
  CallableF
    'KFn
    (ReceiverHeaderF ty)
    SimpleVarIdentifier
    (CodeBlockF stmt expr)
    ty

type OpDefinitionF stmt ty expr =
  CallableF
    'KOp
    UnitF
    (AnyVarIdentifier ty)
    (CodeBlockF stmt expr)
    ty

type OpInfixDefinitionF stmt ty expr =
  CallableF
    'KOp
    (ReceiverHeaderF ty)
    (AnyVarIdentifier ty)
    (CodeBlockF stmt expr)
    ty
