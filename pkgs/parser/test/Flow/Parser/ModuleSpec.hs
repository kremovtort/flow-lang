module Flow.Parser.ModuleSpec (spec) where

import "base" Data.Bifunctor qualified as Bifunctor
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Callable qualified as Callable
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Constraint qualified as Constraint
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Expr qualified as Expr
import Flow.AST.Surface.Literal qualified as Lit
import Flow.AST.Surface.Module qualified as M
import Flow.AST.Surface.Pattern qualified as Pat
import Flow.AST.Surface.Syntax qualified as Syn
import Flow.AST.Surface.Type qualified as Ty
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Module qualified as PMod

type ModuleItem = M.ModuleItemF Surface.Mod Surface.LHSExpression Surface.PatternSimple Surface.Pattern Surface.Type Surface.Expression

modIdent :: Text -> C.ModuleIdentifier ()
modIdent name = C.ModuleIdentifier{name, ann = ()}

simpleVar :: Text -> C.SimpleVarIdentifier ()
simpleVar name = C.SimpleVarIdentifier{name, ann = ()}

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty =
        Ty.TyIdentifierF
          C.AnyTypeIdentifier
            { qualifier = Vector.empty
            , qualifierAnn = Nothing
            , identifier = C.SimpleTypeIdentifier{name, ann = ()}
            , identifierAnn = ()
            , ann = ()
            }
    , ann = ()
    }

modDecl :: Text -> Surface.Mod ()
modDecl name =
  Surface.Mod
    { mod = M.ModDeclaration (modIdent name)
    , ann = ()
    }

modDef :: Text -> [M.UseClause ()] -> [ModuleItem ()] -> Surface.Mod ()
modDef name uses items =
  Surface.Mod
    { mod =
        M.ModDefinition
          (modIdent name)
          M.ModDefinitionBody{uses = Vector.fromList uses, items = Vector.fromList items}
    , ann = ()
    }

useClauseLeaf :: [Text] -> M.UseClause ()
useClauseLeaf path =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        buildTree [] = M.UseTreeLeaf (modIdent root)
        buildTree [segment] = M.UseTreeLeaf (modIdent segment)
        buildTree (segment : segments) =
          M.UseTreeBranch (modIdent segment) (buildTree segments)
      in M.UseClause
        { root = modIdent root
        , tree = buildTree rest
        , ann = ()
        }

useClauseAs :: [Text] -> Text -> M.UseClause ()
useClauseAs path alias =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        build [] = M.UseTreeLeafAs (modIdent root) (modIdent alias)
        build [segment] = M.UseTreeLeafAs (modIdent segment) (modIdent alias)
        build (segment : segments) = M.UseTreeBranch (modIdent segment) (build segments)
      in M.UseClause
        { root = modIdent root
        , tree = build rest
        , ann = ()
        }

modDefUses :: Text -> [M.UseClause ()] -> Surface.Mod ()
modDefUses name uses = modDef name uses []

structItem :: Text -> ModuleItem ()
structItem name =
  M.ModuleItemStruct
    Decl.StructF
      { name = C.SimpleTypeIdentifier{name, ann = ()}
      , scopeParams = mempty
      , typeParams = mempty
      , fields = mempty
      }

enumItem :: Text -> [Text] -> ModuleItem ()
enumItem name variants =
  M.ModuleItemEnum
    Decl.EnumF
      { name = C.SimpleTypeIdentifier{name, ann = ()}
      , scopeParams = mempty
      , typeParams = mempty
      , variants =
          Decl.EVariantsSimpleF
            ( case NE.fromList
                ( fmap
                    (\vname -> Decl.EnumVariantF{name = C.SimpleTypeIdentifier{name = vname, ann = ()}, enumFields = Nothing, ann = ()})
                    variants
                ) of
                Nothing -> error "enumItem: expected non-empty variants"
                Just ne -> ne
            )
            ()
      }

typeAliasItem :: Text -> Surface.Type () -> ModuleItem ()
typeAliasItem name ty =
  M.ModuleItemTypeAlias
    Constraint.TypeDefinitionF
      { name = C.SimpleTypeIdentifier{name, ann = ()}
      , scopeParams = mempty
      , typeParams =
          Vector.fromList
            [ Constraint.BinderWoConstraintF{name = C.SimpleTypeIdentifier{name = "X", ann = ()}, typeType = Nothing, ann = ()}
            , Constraint.BinderWoConstraintF{name = C.SimpleTypeIdentifier{name = "Y", ann = ()}, typeType = Nothing, ann = ()}
            ]
      , type_ = ty
      , ann = ()
      }

fnItem :: Text -> [(Text, Surface.Type ())] -> Maybe (Surface.Type ()) -> Maybe (Surface.Type ()) -> ModuleItem ()
fnItem name args effects result =
  M.ModuleItemFn
    Callable.CallableF
      { header =
          Callable.CallableHeader
            { receiver = Callable.RecieverFPlain ()
            , name = simpleVar name
            , scopeParams = mempty
            , typeParams = mempty
            , args =
                Vector.fromList
                  ( fmap
                      (Bifunctor.first simpleVar)
                      args
                  )
            , effects = effects
            , result = result
            , whereClauses = mempty
            }
      , body = Syn.CodeBlock{statements = mempty, result = Nothing, ann = ()}
      }

letItem :: Text -> Surface.Type () -> Surface.Expression () -> ModuleItem ()
letItem name ty expr =
  M.ModuleItemLet
    Syn.LetDefinitionF
      { mutability = Nothing
      , lhs = Surface.PatternSimple (Pat.PatternSimpleVarF (simpleVar name) ()) ()
      , lhsAnn = ()
      , lhsType = Just (ty, ())
      , rhs = expr
      , rhsAnn = ()
      , ann = ()
      }

tupleType :: [Surface.Type ()] -> Surface.Type ()
tupleType tys = Surface.Type{ty = Ty.TyTupleF (Vector.fromList tys) (), ann = ()}

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Expr.ELiteral (Lit.LitInteger n ()) (), ann = ()}

rootModuleName :: Text
rootModuleName = "_"

rootModule :: [M.UseClause ()] -> [ModuleItem ()] -> Surface.Mod ()
rootModule = modDef rootModuleName

spec :: Spec
spec = describe "Module parser (minimal subset)" do
  it "parses mod declaration 'mod m;'" do
    testParser "mod m;" PMod.pModule (Just (modDecl "m"))

  it "parses empty mod definition 'mod m { }'" do
    testParser "mod m { }" PMod.pModule (Just (modDef "m" [] []))

  it "parses use leaf 'use std::io;'" do
    let expected = rootModule [useClauseLeaf ["std", "io"]] []
    testParser "use std::io;" PMod.pModule (Just expected)

  it "parses use leaf-as 'use std::io as IO;'" do
    let expected = rootModule [useClauseAs ["std", "io"] "IO"] []
    testParser "use std::io as IO;" PMod.pModule (Just expected)

  it "parses nested use 'use std::{io, fs::{read, write}};'" do
    let nestedTree =
          M.UseTreeNested
            ( Vector.fromList
                [ M.UseTreeLeaf (modIdent "io")
                , M.UseTreeBranch
                    (modIdent "fs")
                    ( M.UseTreeNested
                        ( Vector.fromList
                            [ M.UseTreeLeaf (modIdent "read")
                            , M.UseTreeLeaf (modIdent "write")
                            ]
                        )
                    )
                ]
            )
        useClause = M.UseClause{root = modIdent "std", tree = nestedTree, ann = ()}
    testParser "use std::{io, fs::{read, write}};" PMod.pModule (Just (rootModule [useClause] []))

  it "parses minimal items: struct, enum, type alias, fn, let" do
    let src =
          Text.unlines
            [ "struct S {}"
            , "enum E { A, B }"
            , "type Pair<X, Y> = (X, Y)"
            , "fn add(a: i32, b: i32) -> i32 { }"
            , "let x: i32 = 42;"
            ]
        items =
          [ structItem "S"
          , enumItem "E" ["A", "B"]
          , typeAliasItem "Pair" (tupleType [simpleType "X", simpleType "Y"])
          , fnItem
              "add"
              [ ("a", Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()})
              , ("b", Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()})
              ]
              Nothing
              (Just Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()})
          , letItem "x" (Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()}) (literalInt 42)
          ]
    testParser src PMod.pModule (Just (rootModule [] items))
