module Flow.AST.Surface.Decl where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)

import Flow.AST.Common (Scope, SimpleTypeIdentifier, SimpleVarIdentifier)
import Flow.AST.Type (Type, Binder, BinderWoConstraint, WhereClause, TypeDefinition, Forall)
import Flow.AST.Surface.Callable (FnDeclaration, FnInfixDeclaration, OpDeclaration, OpInfixDeclaration)
import Flow.AST.Surface.Callable.Defs (FnDefinition, FnInfixDefinition, OpDefinition, OpInfixDefinition)
import Flow.AST.Surface.Syntax (LetDefinition)
import Flow.AST.Surface.Fields (Fields)

data Struct = Struct
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector BinderWoConstraint
  , fields :: Vector (Fields Type)
  }
  deriving (Eq, Ord, Show)

data Enum = Enum
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector BinderWoConstraint
  , variants :: EnumVariants
  }
  deriving (Eq, Ord, Show)

data EnumVariants
  = EVariantsSimple (NonEmptyVector EnumVariant)
  | EVariantsGeneralized (NonEmptyVector EnumVariantGeneralized)
  deriving (Eq, Ord, Show)

data EnumVariant = EnumVariant
  { name :: SimpleTypeIdentifier
  , enumFields :: Maybe (Fields Type)
  }
  deriving (Eq, Ord, Show)

data EnumVariantGeneralized
  = EVariantGeneralizedSimple EnumVariantGeneralizedSimple
  | EVariantGeneralizedForall (Forall EnumVariantGeneralizedSimple)
  deriving (Eq, Ord, Show)

data EnumVariantGeneralizedSimple = EnumVariantGeneralizedSimple
  { enumVariant :: EnumVariant
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data Trait = Trait
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector BinderWoConstraint
  , superTraits :: Vector Type
  , traitBody :: Vector TraitItem
  }
  deriving (Eq, Ord, Show)

data TraitItem
  = TItemTypeDeclaration TypeDeclaration
  | TItemLetDeclaration LetDeclaration
  | TItemFnDeclaration FnDeclaration
  | TItemFnInfixDeclaration FnInfixDeclaration
  | TItemOpDeclaration OpDeclaration
  | TItemOpInfixDeclaration OpInfixDeclaration
  | TItemFnDefinition FnDefinition
  | TItemFnInfixDefinition FnInfixDefinition
  | TItemOpDefinition OpDefinition
  | TItemOpInfixDefinition OpInfixDefinition
  deriving (Eq, Ord, Show)

data Impl = Impl
  { scopeParams :: Vector Scope
  , typeParams :: Vector Binder
  , trait :: Type
  , whereClauses :: Vector WhereClause
  , body :: Vector ImplItem
  }
  deriving (Eq, Ord, Show)

data ImplItem
  = ImplItemType TypeDefinition
  | ImplItemLet LetDefinition
  | ImplItemFn FnDefinition
  | ImplItemFnInfix FnInfixDefinition
  | ImplItemOp OpDefinition
  | ImplItemOpInfix OpInfixDefinition
  deriving (Eq, Ord, Show)

data Effect = Effect
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Binder
  , superEffects :: Vector Type
  , whereClauses :: Vector WhereClause
  , effectContent :: Vector EffectItemDeclaration
  }
  deriving (Eq, Ord, Show)

data EffectItemDeclaration
  = EItemLetDeclaration LetDeclaration
  | EItemFn FnDeclaration
  | EItemFnInfix FnInfixDeclaration
  | EItemOp OpDeclaration
  | EItemOpInfix OpInfixDeclaration
  | EItemFnDef FnDefinition
  | EItemFnInfixDef FnInfixDefinition
  | EItemOpDef OpDefinition
  | EItemOpInfixDef OpInfixDefinition
  deriving (Eq, Ord, Show)

data TypeDeclaration = TypeDeclaration
  { name :: SimpleTypeIdentifier
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data LetDeclaration = LetDeclaration
  { name :: SimpleVarIdentifier
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)
