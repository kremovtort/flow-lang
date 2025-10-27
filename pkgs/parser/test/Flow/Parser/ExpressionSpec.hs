module Flow.Parser.ExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Data.Functor ((<&>))
import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Expr qualified as Surface
import Flow.AST.Surface.Literal qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Type qualified as Surface.Type
import Flow.Parser.Expr qualified as PExpr
import Flow.Parser.Helpers (testParser)

wildcard :: Surface.Expression ()
wildcard = Surface.Expression{expr = Surface.EWildcard, ann = ()}

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Surface.ELiteral (Surface.LitInteger n), ann = ()}

var :: Text -> Surface.Expression ()
var name =
  Surface.Expression
    { expr =
        Surface.EVar
          Surface.AnyVarIdentifier
            { qualifier = mempty
            , typeQualifier = Nothing
            , identifier = Surface.SimpleVarIdentifier{name, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

parens :: Surface.Expression () -> Surface.Expression ()
parens inner = Surface.Expression{expr = Surface.EParens inner, ann = ()}

unOp :: Surface.UnOp () -> Surface.Expression () -> Surface.Expression ()
unOp op expr =
  Surface.Expression
    { expr = Surface.EUnOp Surface.UnOpExpression{op, operand = expr}
    , ann = ()
    }

binOp :: Surface.BinOp () -> Surface.Expression () -> Surface.Expression () -> Surface.Expression ()
binOp op l r =
  Surface.Expression
    { expr = Surface.EBinOp Surface.BinOpExpression{op, left = l, right = r, ann = ()}
    , ann = ()
    }

tupleExpr :: [Surface.Expression ()] -> Surface.Expression ()
tupleExpr exprs =
  let neVec = case NE.fromList exprs of
        Nothing -> error "Expected non-empty tuple"
        Just v -> v
   in Surface.Expression{expr = Surface.ETuple neVec, ann = ()}

callUnnamed :: Text -> [Surface.Expression ()] -> Surface.Expression ()
callUnnamed fname args =
  Surface.Expression
    { expr =
        Surface.EAppF
          Surface.AppF
            { callee = var fname
            , typeParams = Nothing
            , args = Surface.AppArgsUnnamedF (Vector.fromList args)
            , argsAnn = ()
            , withEffects = Nothing
            }
    , ann = ()
    }

callNamed :: Text -> [(Text, Surface.Expression ())] -> Surface.Expression ()
callNamed fname args =
  Surface.Expression
    { expr =
        Surface.EAppF
          Surface.AppF
            { callee = var fname
            , typeParams = Nothing
            , args =
                Surface.AppArgsNamedF
                  ( Vector.fromList
                      ( args <&> \(name, expr) ->
                          Surface.ArgNamedF
                            { name = Surface.SimpleVarIdentifier name ()
                            , optional = Nothing
                            , value = Just expr
                            , ann = ()
                            }
                      )
                  )
            , argsAnn = ()
            , withEffects = Nothing
            }
    , ann = ()
    }

callWithParams :: Text -> [Surface.ScopeIdentifier ()] -> [Surface.Type ()] -> [Surface.Expression ()] -> Surface.Expression ()
callWithParams fname scopes types args =
  Surface.Expression
    { expr =
        Surface.EAppF
          Surface.AppF
            { callee = var fname
            , typeParams =
                if length scopes + length types > 0
                  then
                    Just
                      Surface.BindersF
                        { scopes = Vector.fromList $ Surface.ScopeBinderWoConstraintsF <$> scopes
                        , types = Vector.fromList $ Surface.BinderAppF <$> types
                        , ann = ()
                        }
                  else Nothing
            , args = Surface.AppArgsUnnamedF (Vector.fromList args)
            , argsAnn = ()
            , withEffects = Nothing
            }
    , ann = ()
    }

scopeIdent :: Text -> Surface.ScopeIdentifier ()
scopeIdent name = Surface.ScopeIdentifier{name, ann = ()}

typeVar :: Text -> Surface.Type ()
typeVar name =
  Surface.Type
    { ty =
        Surface.Type.TyIdentifierF
          Surface.AnyTypeIdentifier
            { qualifier = Nothing
            , typeQualifier = Nothing
            , identifier = Surface.SimpleTypeIdentifier{name, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

dotExpr :: Surface.Expression () -> Text -> Surface.Expression ()
dotExpr base field =
  Surface.Expression
    { expr =
        Surface.EDotAccess
          base
          Surface.AnyVarIdentifier
            { qualifier = mempty
            , typeQualifier = Nothing
            , identifier = Surface.SimpleVarIdentifier{name = field, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

indexExpr :: Surface.Expression () -> Surface.Expression () -> Surface.Expression ()
indexExpr arr idx = Surface.Expression{expr = Surface.EIndex arr idx, ann = ()}

patternSimpleVar :: Bool -> Text -> Surface.PatternSimple ()
patternSimpleVar mut name =
  Surface.PatternSimple
    { patternSimple =
        Surface.PatSimVarF
          ( Surface.PatternVariableF
              { mut = if mut then Just () else Nothing
              , name = Surface.SimpleVarIdentifier{name, ann = ()}
              , ann = ()
              }
          )
    , ann = ()
    }

letStatement :: Text -> Surface.Expression () -> Surface.Statement ()
letStatement name rhsExpr =
  Surface.Statement
    { stmt =
        Surface.SLetF
          Surface.LetDefinitionF
            { lhs = patternSimpleVar False name
            , lhsType = Nothing
            , rhs = rhsExpr
            , ann = ()
            }
    , ann = ()
    }

blockExpr :: [Surface.Statement ()] -> Maybe (Surface.Expression ()) -> Surface.Expression ()
blockExpr stmts resultExpr =
  Surface.Expression
    { expr =
        Surface.EBlock $
          Surface.CodeBlockF
            { statements = Vector.fromList stmts
            , result = resultExpr
            , ann = ()
            }
    , ann = ()
    }

spec :: Spec
spec = describe @() "Expression parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" PExpr.pExpression (Just wildcard)

  it "parses literal 1" do
    testParser "1" PExpr.pExpression (Just (literalInt 1))

  it "parses variable x" do
    testParser "x" PExpr.pExpression (Just (var "x"))

  it "parses parens (x)" do
    testParser "(x)" PExpr.pExpression (Just (parens (var "x")))

  it "parses unary &x, &mut x, &'s x, -x, !x" do
    let cases =
          [ ("&x", Surface.UnOpTakeRef Nothing ())
          , ("&mut x", Surface.UnOpTakeMutRef Nothing ())
          , ("&'s x", Surface.UnOpTakeRef (Just Surface.ScopeIdentifier{name = "s", ann = ()}) ())
          , ("-x", Surface.UnOpNeg ())
          , ("!x", Surface.UnOpNot ())
          ]
    mapM_
      (\(txt, op) -> testParser txt PExpr.pExpression (Just (unOp op (var "x"))))
      cases

  it "parses binary 1 + 2 * 3" do
    let expected =
          binOp
            (Surface.BinOpAdd ())
            (literalInt 1)
            (binOp (Surface.BinOpMul ()) (literalInt 2) (literalInt 3))
    testParser "1 + 2 * 3" PExpr.pExpression (Just expected)

  it "parses calls f(a, b) and with named args f { x = 1, y = 2 }" do
    testParser "f(a, b)" PExpr.pExpression (Just (callUnnamed "f" [var "a", var "b"]))
    testParser "f { x = 1, y = 2 }" PExpr.pExpression (Just (callNamed "f" [("x", literalInt 1), ("y", literalInt 2)]))

  it "parses call with type/scope params f<'s, T>(a)" do
    let expected = callWithParams "f" [scopeIdent "s"] [typeVar "T"] [var "a"]
    testParser "f<'s, T>(a)" PExpr.pExpression (Just expected)

  it "parses chained access a.b[0]" do
    let expected = indexExpr (dotExpr (var "a") "b") (literalInt 0)
    testParser "a.b[0]" PExpr.pExpression (Just expected)

  it "parses tuple (a, b)" do
    testParser "(a, b)" PExpr.pExpression (Just (tupleExpr [var "a", var "b"]))

  it "parses simple block { let x = 1; x }" do
    let stmt = letStatement "x" (literalInt 1)
        expected = blockExpr [stmt] (Just (var "x"))
    testParser "{ let x = 1; x }" PExpr.pExpression (Just expected)
