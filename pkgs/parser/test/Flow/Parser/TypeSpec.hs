module Flow.Parser.TypeSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "vector" Data.Vector qualified as Vector
import "text" Data.Text (Text)

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Type qualified as Ty
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Type qualified as PType

anyType :: C.SimpleTypeIdentifier () -> C.AnyTypeIdentifier ()
anyType ident =
  C.AnyTypeIdentifier
    { qualifier = Vector.empty
    , qualifierAnn = Nothing
    , identifier = ident
    , identifierAnn = ()
    , ann = ()
    }

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty = Ty.TyIdentifierF (anyType C.SimpleTypeIdentifier{name = name, ann = ()})
    , ann = ()
    }

builtin :: Ty.Builtin -> Surface.Type ()
builtin b = Surface.Type{ty = Ty.TyBuiltinF b (), ann = ()}

ref :: Maybe (C.ScopeIdentifier ()) -> Bool -> Surface.Type () -> Surface.Type ()
ref mscope mut inner =
  Surface.Type
    { ty =
        Ty.TyRefF
          Ty.RefF
            { scope = mscope
            , mutability = mut
            , inner = inner
            , ann = ()
            }
    , ann = ()
    }

fnType :: [Surface.Type ()] -> Maybe (Surface.Type ()) -> Surface.Type () -> Surface.Type ()
fnType args eff res =
  Surface.Type
    { ty =
        Ty.TyFnF
          Ty.FnF
            { args = Vector.fromList args
            , effects = eff
            , result = res
            , ann = ()
            }
    , ann = ()
    }

effectRow :: [Ty.EffectAtomF Surface.Type ()] -> Maybe (C.SimpleVarIdentifier ()) -> Surface.Type ()
effectRow atoms tailVar =
  Surface.Type
    { ty =
        Ty.TyEffectRowF
          Ty.EffectRowF
            { effects = Vector.fromList atoms
            , tailVar = tailVar
            , ann = ()
            }
    , ann = ()
    }

mkSimpleVar :: Text -> C.SimpleVarIdentifier ()
mkSimpleVar n = C.SimpleVarIdentifier{name = n, ann = ()}

spec :: Spec
spec = describe "Type parser (minimal subset)" do
  it "parses builtin bool" do
    testParser "bool" PType.pType (Just (builtin Ty.BuiltinBool))

  it "parses simple identifier Option" do
    testParser "Option" PType.pType (Just (simpleType "Option"))

  it "parses application Option<i32>" do
    let headT = simpleType "Option"
        i32T = builtin Ty.BuiltinI32
        app = Surface.Type
          { ty = Ty.TyAppF Ty.AppF{head = headT, args = Vector.fromList [i32T], ann = ()}
          , ann = ()
          }
    testParser "Option<i32>" PType.pType (Just app)

  it "parses tuple (i32, string)" do
    let tup = Surface.Type{ty = Ty.TyTupleF (Vector.fromList [builtin Ty.BuiltinI32, builtin Ty.BuiltinString]) (), ann = ()}
    testParser "(i32, string)" PType.pType (Just tup)

  it "parses refs &T, &mut T, &'s T, &'s mut T" do
    let tT = simpleType "T"
        s = C.ScopeIdentifier{name = "s", ann = ()}
    testParser "&T" PType.pType (Just (ref Nothing False tT))
    testParser "&mut T" PType.pType (Just (ref Nothing True tT))
    testParser "&'s T" PType.pType (Just (ref (Just s) False tT))
    testParser "&'s mut T" PType.pType (Just (ref (Just s) True tT))

  it "parses fn type fn(i32) -> i32" do
    let t = fnType [builtin Ty.BuiltinI32] Nothing (builtin Ty.BuiltinI32)
    testParser "fn(i32) -> i32" PType.pType (Just t)

  it "parses fn with effect row fn(i32) -> @[IO] i32" do
    let ioType = simpleType "IO"
        row = effectRow [Ty.EAtomTypeF ioType ()] Nothing
        t = fnType [builtin Ty.BuiltinI32] (Just row) (builtin Ty.BuiltinI32)
    testParser "fn(i32) -> @[IO] i32" PType.pType (Just t)

  it "parses short effect row fn(i32) -> @R i32" do
    let row = effectRow [] (Just (mkSimpleVar "R"))
        t = fnType [builtin Ty.BuiltinI32] (Just row) (builtin Ty.BuiltinI32)
    testParser "fn(i32) -> @R i32" PType.pType (Just t)

  it "parses effect row @[IO]" do
    let ioType = simpleType "IO"
        row = effectRow [Ty.EAtomTypeF ioType ()] Nothing
    testParser "@[IO]" PType.pType (Just row)


