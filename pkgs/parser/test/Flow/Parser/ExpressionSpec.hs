module Flow.Parser.ExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE

import Data.Bifunctor qualified as Bifunctor
import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Expr qualified as Expr
import Flow.AST.Surface.Literal qualified as Lit
import Flow.AST.Surface.Pattern qualified as Pat
import Flow.AST.Surface.Syntax qualified as Syn
import Flow.AST.Surface.Type qualified as Ty
import Flow.Parser.Expr qualified as PExpr
import Flow.Parser.Helpers (testParser)

type Statement = Syn.StatementF Surface.LHSExpression Surface.PatternSimple Surface.Pattern Surface.Type Surface.Expression

wildcard :: Surface.Expression ()
wildcard = Surface.Expression{expr = Expr.EWildcard (), ann = ()}

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Expr.ELiteral (Lit.LitInteger n ()) (), ann = ()}

var :: Text -> Surface.Expression ()
var name =
  Surface.Expression
    { expr =
        Expr.EVar
          C.AnyVarIdentifier
            { qualifier = mempty
            , qualifierAnn = Nothing
            , identifier = C.SimpleVarIdentifier{name, ann = ()}
            , identifierAnn = ()
            , ann = ()
            }
          ()
    , ann = ()
    }

parens :: Surface.Expression () -> Surface.Expression ()
parens inner = Surface.Expression{expr = Expr.EParens inner (), ann = ()}

unOp :: Expr.UnOp () -> Surface.Expression () -> Surface.Expression ()
unOp op expr = Surface.Expression{expr = Expr.EUnOp Expr.UnOpExpression{op, operand = expr, ann = ()} (), ann = ()}

binOp :: Expr.BinOp () -> Surface.Expression () -> Surface.Expression () -> Surface.Expression ()
binOp op l r =
  Surface.Expression
    { expr = Expr.EBinOp Expr.BinOpExpression{op, left = l, right = r, ann = ()} ()
    , ann = ()
    }

tupleExpr :: [Surface.Expression ()] -> Surface.Expression ()
tupleExpr exprs =
  let neVec = case NE.fromList exprs of
        Nothing -> error "Expected non-empty tuple"
        Just v -> v
  in Surface.Expression{expr = Expr.ETuple neVec (), ann = ()}

callUnnamed :: Text -> [Surface.Expression ()] -> Surface.Expression ()
callUnnamed fname args =
  Surface.Expression
    { expr =
        Expr.EFnCall
          Expr.FnCallF
            { callee = var fname
            , calleeAnn = ()
            , scopeParams = Nothing
            , typeParams = Nothing
            , typeParamsAnn = ()
            , args = Expr.FnArgsUnnamedF (Vector.fromList args) ()
            , argsAnn = ()
            , withEffects = Nothing
            , withEffectsAnn = ()
            }
          ()
    , ann = ()
    }

callNamed :: Text -> [(Text, Surface.Expression ())] -> Surface.Expression ()
callNamed fname args =
  Surface.Expression
    { expr =
        Expr.EFnCall
          Expr.FnCallF
            { callee = var fname
            , calleeAnn = ()
            , scopeParams = Nothing
            , typeParams = Nothing
            , typeParamsAnn = ()
            , args =
                Expr.FnArgsNamedF
                  ( Vector.fromList
                      ( fmap
                          ( Bifunctor.first
                              (\name -> C.SimpleVarIdentifier{name, ann = ()})
                          )
                          args
                      )
                  )
                  ()
            , argsAnn = ()
            , withEffects = Nothing
            , withEffectsAnn = ()
            }
          ()
    , ann = ()
    }

callWithParams :: Text -> [C.ScopeIdentifier ()] -> [Surface.Type ()] -> [Surface.Expression ()] -> Surface.Expression ()
callWithParams fname scopes types args =
  Surface.Expression
    { expr =
        Expr.EFnCall
          Expr.FnCallF
            { callee = var fname
            , calleeAnn = ()
            , scopeParams = NE.fromList scopes
            , typeParams = NE.fromList types
            , typeParamsAnn = ()
            , args = Expr.FnArgsUnnamedF (Vector.fromList args) ()
            , argsAnn = ()
            , withEffects = Nothing
            , withEffectsAnn = ()
            }
          ()
    , ann = ()
    }

scopeIdent :: Text -> C.ScopeIdentifier ()
scopeIdent name = C.ScopeIdentifier{name, ann = ()}

typeVar :: Text -> Surface.Type ()
typeVar name =
  Surface.Type
    { ty = Ty.TyIdentifierF C.AnyTypeIdentifier
        { qualifier = Vector.empty
        , qualifierAnn = Nothing
        , identifier = C.SimpleTypeIdentifier{name, ann = ()}
        , identifierAnn = ()
        , ann = ()
        }
    , ann = ()
    }

dotExpr :: Surface.Expression () -> Text -> Surface.Expression ()
dotExpr base field =
  Surface.Expression
    { expr =
        Expr.EDotAccess
          base
          C.AnyVarIdentifier
            { qualifier = mempty
            , qualifierAnn = Nothing
            , identifier = C.SimpleVarIdentifier{name = field, ann = ()}
            , identifierAnn = ()
            , ann = ()
            }
          ()
    , ann = ()
    }

indexExpr :: Surface.Expression () -> Surface.Expression () -> Surface.Expression ()
indexExpr arr idx = Surface.Expression{expr = Expr.EIndex arr idx (), ann = ()}

patternSimpleVar :: Text -> Surface.PatternSimple ()
patternSimpleVar name =
  Surface.PatternSimple
    { patternSimple = Pat.PatternSimpleVarF (C.SimpleVarIdentifier{name, ann = ()}) ()
    , ann = ()
    }

letStatement :: Text -> Surface.Expression () -> Statement ()
letStatement name rhsExpr =
  Syn.SLetF
    Syn.LetDefinitionF
      { mutability = Nothing
      , lhs = patternSimpleVar name
      , lhsAnn = ()
      , lhsType = Nothing
      , rhs = rhsExpr
      , rhsAnn = ()
      , ann = ()
      }

blockExpr :: [Statement ()] -> Maybe (Surface.Expression ()) -> Surface.Expression ()
blockExpr stmts resultExpr =
  Surface.Expression
    { expr =
        Expr.EBlock
          Syn.CodeBlock
            { statements = Vector.fromList stmts
            , result = resultExpr
            , ann = ()
            }
          ()
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
          [ ("&x", Expr.UnOpTakeRef Nothing ())
          , ("&mut x", Expr.UnOpTakeMutRef Nothing ())
          , ("&'s x", Expr.UnOpTakeRef (Just C.ScopeIdentifier{name = "s", ann = ()}) ())
          , ("-x", Expr.UnOpNeg ())
          , ("!x", Expr.UnOpNot ())
          ]
    mapM_
      (\(txt, op) -> testParser txt PExpr.pExpression (Just (unOp op (var "x"))))
      cases

  it "parses binary 1 + 2 * 3" do
    let expected =
          binOp
            (Expr.BinOpAdd ())
            (literalInt 1)
            (binOp (Expr.BinOpMul ()) (literalInt 2) (literalInt 3))
    testParser "1 + 2 * 3" PExpr.pExpression (Just expected)

  it "parses calls f(a, b) and with named args f(x = 1, y = 2)" do
    testParser "f(a, b)" PExpr.pExpression (Just (callUnnamed "f" [var "a", var "b"]))
    testParser "f(x = 1, y = 2)" PExpr.pExpression (Just (callNamed "f" [("x", literalInt 1), ("y", literalInt 2)]))

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
