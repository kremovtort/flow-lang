module Flow.AST.Surface.Decl where

import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)
import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)

import Flow.AST.Surface.Common (SimpleTypeIdentifier, SimpleVarIdentifier)
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
import Flow.AST.Surface.Constraint (BindersWoConstraintsF, TypeDefinitionF, WhereBlockF, BindersWConstraintsF)
import Flow.AST.Surface.Syntax (LetDefinitionF, Fields)
import Flow.AST.Surface.Type (TypeF)

data StructF ty ann = StructF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , fields :: Vector (Fields ty ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumF ty ann = EnumF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , variants :: EnumVariantsF ty ann
  , variantsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantsF ty ann
  = EVariantsSimpleF (NonEmptyVector (EnumVariantF ty ann))
  | EVariantsGeneralized (NonEmptyVector (EnumVariantGeneralizedF ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantF ty ann = EnumVariantF
  { name :: SimpleTypeIdentifier ann
  , enumFields :: Maybe (Fields ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedF ty ann = EnumVariantGeneralizedF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , result :: ty ann
  , resultAnn :: ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , whereAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)


data EnumVariantGeneralizedSimpleF ty ann = EnumVariantGeneralizedSimpleF
  { enumVariant :: EnumVariantF ty ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Trait lhsExpr simPat pat ty expr ann = Trait
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , superTraits :: Vector (ty ann)
  , traitBody :: Vector (TraitItem lhsExpr simPat pat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItem lhsExpr simPat pat ty expr ann
  = TItemTypeDeclaration (TypeDeclarationF ty ann)
  | TItemLetDeclaration (LetDeclarationF ty ann)
  | TItemFnDeclaration (FnDeclarationF ty ann)
  | TItemFnInfixDeclaration (FnInfixDeclarationF ty ann)
  | TItemOpDeclaration (OpDeclarationF ty ann)
  | TItemOpInfixDeclaration (OpInfixDeclarationF ty ann)
  | TItemFnDefinition (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | TItemFnInfixDefinition (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | TItemOpDefinition (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | TItemOpInfixDefinition (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Impl lhsExpr simPat pat ty expr ann = Impl
  { typeParams :: Maybe (BindersWConstraintsF ty ann)
  , trait :: TypeF ty ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , body :: Vector (ImplItemF lhsExpr simPat pat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplItemF lhsExpr simPat pat ty expr ann
  = ImplItemType (TypeDefinitionF ty ann)
  | ImplItemLet (LetDefinitionF simPat ty expr ann)
  | ImplItemFn (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | ImplItemFnInfix (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | ImplItemOp (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | ImplItemOpInfix (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectF lhsExpr simPat pat ty expr ann = EffectF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , superEffects :: Vector (ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , effectContent :: Vector (EffectItemDeclarationF lhsExpr simPat pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemDeclarationF lhsExpr simPat pat ty expr ann
  = EItemLetDeclaration (LetDeclarationF ty ann)
  | EItemFn (FnDeclarationF ty ann)
  | EItemFnInfix (FnInfixDeclarationF ty ann)
  | EItemOp (OpDeclarationF ty ann)
  | EItemOpInfix (OpInfixDeclarationF ty ann)
  | EItemFnDef (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | EItemFnInfixDef (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | EItemOpDef (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | EItemOpInfixDef (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeDeclarationF ty ann = TypeDeclarationF
  { name :: SimpleTypeIdentifier ann
  , type_ :: ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetDeclarationF ty ann = LetDeclarationF
  { name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
