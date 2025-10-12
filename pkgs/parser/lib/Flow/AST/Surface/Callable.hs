{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, StandaloneDeriving #-}

module Flow.AST.Surface.Callable where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)

import Flow.AST.Common (Scope, SimpleVarIdentifier)
import Flow.AST.Type (Type, Binder, WhereClause)

-- | Kinds for indexing callables
data CallKind = KFn | KOp
data Fixity = Plain | Infix
data Phase = Decl | Def

-- | Receiver header for infix calls
data ReceiverHeader (f :: Fixity) where
  NoRecv  :: ReceiverHeader 'Plain
  HasRecv :: { calleeScopeParams :: Vector Scope
             , calleeTypeParams  :: Vector Binder
             , calleeName        :: SimpleVarIdentifier
             , calleeType        :: Type
             } -> ReceiverHeader 'Infix

-- | Common header for all callable entities
data CallableHeader (f :: Fixity) = CallableHeader
  { receiver     :: ReceiverHeader f
  , name         :: SimpleVarIdentifier
  , scopeParams  :: Vector Scope
  , typeParams   :: Vector Binder
  , args         :: Vector (SimpleVarIdentifier, Type)
  , effects      :: Maybe Type
  , result       :: Type
  , whereClauses :: Vector WhereClause
  }

-- | GADT for all callable definitions and declarations
data Callable (k :: CallKind) (f :: Fixity) (p :: Phase) body where
  -- Declarations (no body)
  FnDecl  :: CallableHeader f -> Callable 'KFn f 'Decl body
  OpDecl  :: CallableHeader f -> Callable 'KOp f 'Decl body

  -- Definitions (with body)
  FnDef   :: CallableHeader f -> body -> Callable 'KFn f 'Def body
  OpDef   :: CallableHeader f -> body -> Callable 'KOp f 'Def body

-- | Type synonyms for backward compatibility
-- Declarations fix the body type to unit to avoid propagating a type parameter
type FnDeclaration       = Callable 'KFn 'Plain 'Decl ()
type FnInfixDeclaration  = Callable 'KFn 'Infix 'Decl ()
type OpDeclaration       = Callable 'KOp 'Plain 'Decl ()
type OpInfixDeclaration  = Callable 'KOp 'Infix 'Decl ()

-- | Parameterized synonyms for definitions by body type
type FnDefinitionOf body        = Callable 'KFn 'Plain 'Def body
type FnInfixDefinitionOf body   = Callable 'KFn 'Infix 'Def body
type OpDefinitionOf body        = Callable 'KOp 'Plain 'Def body
type OpInfixDefinitionOf body   = Callable 'KOp 'Infix 'Def body

-- Standalone deriving for GADTs and indexed headers

deriving instance Eq (ReceiverHeader 'Plain)
deriving instance Ord (ReceiverHeader 'Plain)
deriving instance Show (ReceiverHeader 'Plain)

deriving instance Eq (ReceiverHeader 'Infix)
deriving instance Ord (ReceiverHeader 'Infix)
deriving instance Show (ReceiverHeader 'Infix)

deriving instance Eq (CallableHeader 'Plain)
deriving instance Ord (CallableHeader 'Plain)
deriving instance Show (CallableHeader 'Plain)

deriving instance Eq (CallableHeader 'Infix)
deriving instance Ord (CallableHeader 'Infix)
deriving instance Show (CallableHeader 'Infix)

deriving instance Eq (Callable 'KFn 'Plain 'Decl body)
deriving instance Ord (Callable 'KFn 'Plain 'Decl body)
deriving instance Show (Callable 'KFn 'Plain 'Decl body)

deriving instance Eq (Callable 'KFn 'Infix 'Decl body)
deriving instance Ord (Callable 'KFn 'Infix 'Decl body)
deriving instance Show (Callable 'KFn 'Infix 'Decl body)

deriving instance Eq body => Eq (Callable 'KFn 'Plain 'Def body)
deriving instance Ord body => Ord (Callable 'KFn 'Plain 'Def body)
deriving instance Show body => Show (Callable 'KFn 'Plain 'Def body)

deriving instance Eq body => Eq (Callable 'KFn 'Infix 'Def body)
deriving instance Ord body => Ord (Callable 'KFn 'Infix 'Def body)
deriving instance Show body => Show (Callable 'KFn 'Infix 'Def body)

deriving instance Eq (Callable 'KOp 'Plain 'Decl body)
deriving instance Ord (Callable 'KOp 'Plain 'Decl body)
deriving instance Show (Callable 'KOp 'Plain 'Decl body)

deriving instance Eq (Callable 'KOp 'Infix 'Decl body)
deriving instance Ord (Callable 'KOp 'Infix 'Decl body)
deriving instance Show (Callable 'KOp 'Infix 'Decl body)

deriving instance Eq body => Eq (Callable 'KOp 'Plain 'Def body)
deriving instance Ord body => Ord (Callable 'KOp 'Plain 'Def body)
deriving instance Show body => Show (Callable 'KOp 'Plain 'Def body)

deriving instance Eq body => Eq (Callable 'KOp 'Infix 'Def body)
deriving instance Ord body => Ord (Callable 'KOp 'Infix 'Def body)
deriving instance Show body => Show (Callable 'KOp 'Infix 'Def body)
