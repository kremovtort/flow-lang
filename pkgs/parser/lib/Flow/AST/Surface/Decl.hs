module Flow.AST.Surface.Decl where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

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
import Flow.AST.Surface.Common (SimpleTypeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (
  BindersWConstraintsF,
  BindersWoConstraintsF,
  TypeDefinitionF,
  WhereBlockF,
 )
import Flow.AST.Surface.Syntax (LetDefinitionF)
import Flow.AST.Surface.Type (TypeF)

data StructF ty ann = StructF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , fields :: Vector (FieldsDeclF ty ann)
  , fieldsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldsDeclF ty ann
  = FieldsDeclNamedF (Vector (FieldDeclF ty ann))
  | FieldsDeclTupleF (Vector (ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldDeclF ty ann = FieldDeclF
  { pub :: Maybe (PubF ann)
  , name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  , ann :: ann
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
  , fields :: Maybe (FieldsDeclF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedF ty ann = EnumVariantGeneralizedF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , fields :: Maybe (FieldsDeclF ty ann)
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
  , traitBody :: Vector (TraitItemF lhsExpr simPat pat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemF lhsExpr simPat pat ty expr ann = TraitItemF
  { pub :: Maybe (PubF ann)
  , item :: TraitItemVariantF lhsExpr simPat pat ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemVariantF lhsExpr simPat pat ty expr ann
  = TItemVarTypeDeclarationF (TypeDeclarationF ty ann)
  | TItemVarLetDeclarationF (LetDeclarationF ty ann)
  | TItemVarFnDeclarationF (FnDeclarationF ty ann)
  | TItemVarFnInfixDeclarationF (FnInfixDeclarationF ty ann)
  | TItemVarOpDeclarationF (OpDeclarationF ty ann)
  | TItemVarOpInfixDeclarationF (OpInfixDeclarationF ty ann)
  | TItemVarFnDefinitionF (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | TItemVarFnInfixDefinitionF (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | TItemVarOpDefinitionF (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | TItemVarOpInfixDefinitionF (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Impl lhsExpr simPat pat ty expr ann = Impl
  { typeParams :: Maybe (BindersWConstraintsF ty ann)
  , trait :: TypeF ty ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , body :: Vector (ImplItemVariantF lhsExpr simPat pat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplItemVariantF lhsExpr simPat pat ty expr ann
  = IItemVarTypeF (TypeDefinitionF ty ann)
  | IItemVarLetF (LetDefinitionF simPat ty expr ann)
  | IItemVarFnF (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | IItemVarFnInfixF (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | IItemVarOpF (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | IItemVarOpInfixF (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectF lhsExpr simPat pat ty expr ann = EffectF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , superEffects :: Vector (ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , effectContent :: Vector (EffectItemDeclarationF lhsExpr simPat pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemDeclarationF lhsExpr simPat pat ty expr ann = EffectItemDeclarationF
  { pub :: Maybe (PubF ann)
  , variant :: EffectItemDeclarationVariantF lhsExpr simPat pat ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemDeclarationVariantF lhsExpr simPat pat ty expr ann
  = EItemDeclVarLet (LetDeclarationF ty ann)
  | EItemDeclVarFn (FnDeclarationF ty ann)
  | EItemDeclVarFnInfix (FnInfixDeclarationF ty ann)
  | EItemDeclVarOp (OpDeclarationF ty ann)
  | EItemDeclVarOpInfix (OpInfixDeclarationF ty ann)
  | EItemDeclVarFnDef (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | EItemDeclVarFnInfixDef (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | EItemDeclVarOpDef (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | EItemDeclVarOpInfixDef (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
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

data PubF ann
  = PubPubF ann
  | PubPackageF ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
