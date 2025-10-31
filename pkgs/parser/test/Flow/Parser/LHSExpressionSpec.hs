module Flow.Parser.LHSExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "text" Data.Text (Text)

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Expr qualified as Surface
import Flow.AST.Surface.Syntax qualified as Syn
import Flow.Parser (pLhsExpression)
import Flow.Parser.Helpers (testParser)

lhsVar :: Text -> Surface.LHSExpression ()
lhsVar name =
  Surface.LHSExpression
    { lhsExpression = Syn.LHSEVar Surface.SimpleVarIdentifier{name, ann = ()}
    , ann = ()
    }

lhsIndex :: Surface.LHSExpression () -> Surface.Expression () -> Surface.LHSExpression ()
lhsIndex lhs rhs =
  Surface.LHSExpression
    { lhsExpression = Syn.LHSEIndex lhs rhs
    , ann = ()
    }

lhsDot :: Surface.LHSExpression () -> Text -> Surface.LHSExpression ()
lhsDot lhs field =
  Surface.LHSExpression
    { lhsExpression =
        Syn.LHSEDotAccess
          lhs
          Surface.SimpleVarIdentifier
            { name = field
            , ann = ()
            }
    , ann = ()
    }

lhsDeref :: Surface.Expression () -> Surface.LHSExpression ()
lhsDeref expr =
  Surface.LHSExpression
    { lhsExpression = Syn.LHSEUnOp (Syn.LHSUnOpExpressionDeref expr)
    , ann = ()
    }

exprVar :: Text -> Surface.Expression ()
exprVar name =
  Surface.Expression
    { expr =
        Surface.EVar
          ( Surface.AnyVarIdentifier
              { qualifier = Nothing
              , typeQualifier = Nothing
              , identifier = Surface.SimpleVarIdentifier{name, ann = ()}
              , ann = ()
              }
          )
    , ann = ()
    }

spec :: Spec
spec = describe "LHSExpression parser (minimal subset)" do
  it "parses variable x" do
    testParser "x" pLhsExpression (Just (lhsVar "x"))

  it "parses index a[b]" do
    let expected = lhsIndex (lhsVar "a") (exprVar "b")
    testParser "a[b]" pLhsExpression (Just expected)

  it "parses dot access a.b" do
    let expected = lhsDot (lhsVar "a") "b"
    testParser "a.b" pLhsExpression (Just expected)

  it "parses multiple dot accesses a.b.c" do
    let expected = lhsDot (lhsDot (lhsVar "a") "b") "c"
    testParser "a.b.c" pLhsExpression (Just expected)

  it "parses deref *a" do
    let expected = lhsDeref (exprVar "a")
    testParser "*a" pLhsExpression (Just expected)
