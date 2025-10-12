module Flow.AST.Surface.Decl where

import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Common (ScopeIdentifier, SimpleTypeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Callable (
  FnDeclarationF,
  FnDefinitionF,
  FnInfixDeclarationF,
  FnInfixDefinitionF,
  OpDeclarationF,
  OpDefinitionF,
  OpInfixDeclarationF,
  OpInfixDefinitionF,
 )
import Flow.AST.Surface.Constraint (BinderF, BinderWoConstraintF, TypeDefinitionF, WhereClauseF)
import Flow.AST.Surface.Fields (Fields)
import Flow.AST.Surface.Syntax (LetDefinitionF)
import Flow.AST.Surface.Type (ForallF, TypeF)

data StructF ty ann = StructF
  { name :: SimpleTypeIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderWoConstraintF ty ann)
  , fields :: Vector (Fields ty ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EnumF ty ann = EnumF
  { name :: SimpleTypeIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderWoConstraintF ty ann)
  , variants :: EnumVariantsF ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EnumVariantsF ty ann
  = EVariantsSimpleF (NonEmptyVector (EnumVariantF ty ann)) ann
  | EVariantsGeneralized (NonEmptyVector (EnumVariantGeneralizedF ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EnumVariantF ty ann = EnumVariantF
  { name :: SimpleTypeIdentifier ann
  , enumFields :: Maybe (Fields ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EnumVariantGeneralizedF ty ann
  = EVariantGeneralizedSimpleF (EnumVariantGeneralizedSimpleF ty ann)
  | EVariantGeneralizedForallF (ForallF ty ann) (EnumVariantGeneralizedSimpleF ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EnumVariantGeneralizedSimpleF ty ann = EnumVariantGeneralizedSimpleF
  { enumVariant :: EnumVariantF ty ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Trait lhsExpr pat ty expr ann = Trait
  { name :: SimpleTypeIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderWoConstraintF ty ann)
  , superTraits :: Vector (ty ann)
  , traitBody :: Vector (TraitItem lhsExpr pat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TraitItem lhsExpr pat ty expr ann
  = TItemTypeDeclaration (TypeDeclarationF ty ann)
  | TItemLetDeclaration (LetDeclarationF ty ann)
  | TItemFnDeclaration (FnDeclarationF ty ann)
  | TItemFnInfixDeclaration (FnInfixDeclarationF ty ann)
  | TItemOpDeclaration (OpDeclarationF ty ann)
  | TItemOpInfixDeclaration (OpInfixDeclarationF ty ann)
  | TItemFnDefinition (FnDefinitionF lhsExpr pat ty expr ann)
  | TItemFnInfixDefinition (FnInfixDefinitionF lhsExpr pat ty expr ann)
  | TItemOpDefinition (OpDefinitionF lhsExpr pat ty expr ann)
  | TItemOpInfixDefinition (OpInfixDefinitionF lhsExpr pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Impl lhsExpr pat ty expr ann = Impl
  { scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderF ty ann)
  , trait :: TypeF ty ann
  , whereClauses :: Vector (WhereClauseF ty ann)
  , body :: Vector (ImplItemF lhsExpr pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ImplItemF lhsExpr pat ty expr ann
  = ImplItemType (TypeDefinitionF ty ann)
  | ImplItemLet (LetDefinitionF pat ty expr ann)
  | ImplItemFn (FnDefinitionF lhsExpr pat ty expr ann)
  | ImplItemFnInfix (FnInfixDefinitionF lhsExpr pat ty expr ann)
  | ImplItemOp (OpDefinitionF lhsExpr pat ty expr ann)
  | ImplItemOpInfix (OpInfixDefinitionF lhsExpr pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EffectF lhsExpr pat ty expr ann = EffectF
  { name :: SimpleTypeIdentifier ann
  , scopeParams :: Vector (ScopeIdentifier ann)
  , typeParams :: Vector (BinderF ty ann)
  , superEffects :: Vector (ty ann)
  , whereClauses :: Vector (WhereClauseF ty ann)
  , effectContent :: Vector (EffectItemDeclarationF lhsExpr pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data EffectItemDeclarationF lhsExpr pat ty expr ann
  = EItemLetDeclaration (LetDeclarationF ty ann)
  | EItemFn (FnDeclarationF ty ann)
  | EItemFnInfix (FnInfixDeclarationF ty ann)
  | EItemOp (OpDeclarationF ty ann)
  | EItemOpInfix (OpInfixDeclarationF ty ann)
  | EItemFnDef (FnDefinitionF lhsExpr pat ty expr ann)
  | EItemFnInfixDef (FnInfixDefinitionF lhsExpr pat ty expr ann)
  | EItemOpDef (OpDefinitionF lhsExpr pat ty expr ann)
  | EItemOpInfixDef (OpInfixDefinitionF lhsExpr pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeDeclarationF ty ann = TypeDeclarationF
  { name :: SimpleTypeIdentifier ann
  , type_ :: ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LetDeclarationF ty ann = LetDeclarationF
  { name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
