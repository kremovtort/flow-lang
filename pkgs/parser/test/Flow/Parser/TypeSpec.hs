module Flow.Parser.TypeSpec (spec) where

import "base" Data.Maybe (fromJust)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Type qualified as PType

anyType :: Surface.SimpleTypeIdentifier () -> Surface.AnyTypeIdentifier ty ()
anyType ident =
  Surface.AnyTypeIdentifier
    { qualifier = Nothing
    , typeQualifier = Nothing
    , identifier = ident
    , ann = ()
    }

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty = Surface.TyIdentifierF (anyType Surface.SimpleTypeIdentifier{name = name, ann = ()})
    , ann = ()
    }

builtin :: Surface.Builtin -> Surface.Type ()
builtin b = Surface.Type{ty = Surface.TyBuiltinF b (), ann = ()}

ref :: Maybe (Surface.ScopeIdentifier ()) -> Bool -> Surface.Type () -> Surface.Type ()
ref mscope mut inner =
  Surface.Type
    { ty =
        Surface.TyAppF
          Surface.AppF
            { head =
                Surface.Type
                  { ty =
                      Surface.TyRefF
                        Surface.RefF
                          { scope = mscope
                          , mutability = if mut then Just () else Nothing
                          , ann = ()
                          }
                  , ann = ()
                  }
            , args =
                Surface.BindersF
                  { scopes = Vector.empty
                  , types = Vector.singleton $ Surface.BinderAppF inner
                  , ann = ()
                  }
            , ann = ()
            }
    , ann = ()
    }

fnType :: [Surface.Type ()] -> Maybe (Surface.Type ()) -> Surface.Type () -> Surface.Type ()
fnType args eff res =
  Surface.Type
    { ty =
        Surface.TyFnF
          Surface.FnF
            { args = Vector.fromList args
            , argsAnn = ()
            , effects = (,()) <$> eff
            , result = res
            , resultAnn = ()
            , ann = ()
            }
    , ann = ()
    }

effectRow :: [Surface.EffectAtomF Surface.Type ()] -> Maybe (Surface.AnyTypeIdentifier Surface.Type ()) -> Surface.Type ()
effectRow atoms tailVar =
  Surface.Type
    { ty =
        Surface.TyEffectRowF
          Surface.EffectRowF
            { effects = Vector.fromList atoms
            , tailVar = tailVar
            , ann = ()
            }
    , ann = ()
    }

mkSimpleVar :: Text -> Surface.SimpleVarIdentifier ()
mkSimpleVar n = Surface.SimpleVarIdentifier{name = n, ann = ()}

spec :: Spec
spec = describe "Type parser (minimal subset)" do
  it "parses builtin bool" do
    testParser "bool" PType.pType (Just (builtin Surface.BuiltinBool))

  it "parses simple identifier Option" do
    testParser "Option" PType.pType (Just (simpleType "Option"))

  it "parses application Option<i32>" do
    let headT = simpleType "Option"
        i32T = builtin Surface.BuiltinI32
        app =
          Surface.Type
            { ty =
                Surface.TyAppF
                  Surface.AppF
                    { head = headT
                    , args =
                        Surface.BindersF
                          { scopes = Vector.empty
                          , types = Vector.singleton $ Surface.BinderAppF i32T
                          , ann = ()
                          }
                    , ann = ()
                    }
            , ann = ()
            }
    testParser "Option<i32>" PType.pType (Just app)

  it "parses tuple (i32, string)" do
    let tup =
          Surface.Type
            { ty =
                Surface.TyTupleF
                  ( fromJust $
                      NonEmptyVector.fromList [builtin Surface.BuiltinI32, builtin Surface.BuiltinString]
                  )
                  ()
            , ann = ()
            }
    testParser "(i32, string)" PType.pType (Just tup)

  describe "parses applied refs" do
    let tT = simpleType "T"
        s = Surface.ScopeIdentifier{name = "s", ann = ()}

    it "&T" do
      testParser "&T" PType.pType (Just (ref Nothing False tT))

    it "&mut T" do
      testParser "&mut T" PType.pType (Just (ref Nothing True tT))

    it "&'s T" do
      testParser "&'s T" PType.pType (Just (ref (Just s) False tT))

    it "&'s mut T" do
      testParser "&'s mut T" PType.pType (Just (ref (Just s) True tT))

  it "parses fn type fn(i32) -> i32" do
    let t = fnType [builtin Surface.BuiltinI32] Nothing (builtin Surface.BuiltinI32)
    testParser "fn(i32) -> i32" PType.pType (Just t)

  it "parses fn with effect row fn(i32) -> @[IO] i32" do
    let ioType = simpleType "IO"
        row = effectRow [Surface.EAtomTypeF ioType ()] Nothing
        t = fnType [builtin Surface.BuiltinI32] (Just row) (builtin Surface.BuiltinI32)
    testParser "fn(i32) -> @[IO] i32" PType.pType (Just t)

  it "parses short effect row fn(i32) -> @R i32" do
    let row = simpleType "R"
        t = fnType [builtin Surface.BuiltinI32] (Just row) (builtin Surface.BuiltinI32)
    testParser "fn(i32) -> @R i32" PType.pType (Just t)

  it "parses effect row @[IO]" do
    let ioType = simpleType "IO"
        row = effectRow [Surface.EAtomTypeF ioType ()] Nothing
    testParser "@[IO]" PType.pType (Just row)
