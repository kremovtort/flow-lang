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
  , fields :: FieldsDeclF ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldsDeclF ty ann
  = FieldsDeclNamedF (Vector (FieldDeclF ty ann))
  | FieldsDeclTupleF (Vector (ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldDeclF ty ann = FieldDeclF
  { pub :: Maybe (Pub ann)
  , name :: SimpleVarIdentifier ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumF ty ann = EnumF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , variants :: EnumVariantsF ty ann
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
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedSimpleF ty ann = EnumVariantGeneralizedSimpleF
  { enumVariant :: EnumVariantF ty ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Trait stmt ty expr ann = Trait
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , superTraits :: Vector (ty ann)
  , traitBody :: Vector (TraitItemF stmt ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemF stmt ty expr ann = TraitItemF
  { pub :: Maybe (Pub ann)
  , item :: TraitItemVariantF stmt ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemVariantF stmt ty expr ann
  = TItemVarTypeDeclarationF (TypeDeclarationF ty ann)
  | TItemVarLetDeclarationF (LetDeclarationF ty ann)
  | TItemVarFnDeclarationF (FnDeclarationF ty ann)
  | TItemVarFnInfixDeclarationF (FnInfixDeclarationF ty ann)
  | TItemVarOpDeclarationF (OpDeclarationF ty ann)
  | TItemVarOpInfixDeclarationF (OpInfixDeclarationF ty ann)
  | TItemVarFnDefinitionF (FnDefinitionF stmt ty expr ann)
  | TItemVarFnInfixDefinitionF (FnInfixDefinitionF stmt ty expr ann)
  | TItemVarOpDefinitionF (OpDefinitionF stmt ty expr ann)
  | TItemVarOpInfixDefinitionF (OpInfixDefinitionF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data Impl stmt simPat ty expr ann = Impl
  { typeParams :: Maybe (BindersWConstraintsF ty ann)
  , trait :: TypeF ty ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , body :: Vector (ImplItemVariantF stmt simPat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplItemVariantF stmt simPat ty expr ann
  = IItemVarTypeF (TypeDefinitionF ty ann)
  | IItemVarLetF (LetDefinitionF simPat ty expr ann)
  | IItemVarFnF (FnDefinitionF stmt ty expr ann)
  | IItemVarFnInfixF (FnInfixDefinitionF stmt ty expr ann)
  | IItemVarOpF (OpDefinitionF stmt ty expr ann)
  | IItemVarOpInfixF (OpInfixDefinitionF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectF stmt ty expr ann = EffectF
  { name :: SimpleTypeIdentifier ann
  , typeParams :: Maybe (BindersWConstraintsF ty ann)
  , superEffects :: Vector (ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , effectContent :: Vector (EffectItemDeclarationF stmt ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemDeclarationF stmt ty expr ann = EffectItemDeclarationF
  { pub :: Maybe (Pub ann)
  , variant :: EffectItemDeclarationVariantF stmt ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemDeclarationVariantF stmt ty expr ann
  = EItemDeclVarLet (LetDeclarationF ty ann)
  | EItemDeclVarFn (FnDeclarationF ty ann)
  | EItemDeclVarFnInfix (FnInfixDeclarationF ty ann)
  | EItemDeclVarOp (OpDeclarationF ty ann)
  | EItemDeclVarOpInfix (OpInfixDeclarationF ty ann)
  | EItemDeclVarFnDef (FnDefinitionF stmt ty expr ann)
  | EItemDeclVarFnInfixDef (FnInfixDefinitionF stmt ty expr ann)
  | EItemDeclVarOpDef (OpDefinitionF stmt ty expr ann)
  | EItemDeclVarOpInfixDef (OpInfixDefinitionF stmt ty expr ann)
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

data Pub ann
  = PubPub
  | PubPackage ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
