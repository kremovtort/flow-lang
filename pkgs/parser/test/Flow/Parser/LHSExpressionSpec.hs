module Flow.Parser.LHSExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "text" Data.Text (Text)

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Expr qualified as Surface
import Flow.AST.Surface.Syntax qualified as Syn
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Syntax qualified as PSyn
import Flow.Parser.Common (Parser, SourceRegion(..))
import Flow.Parser.Expr qualified as PExpr

pLHSExpression :: Parser (Surface.LHSExpression SourceRegion)
pLHSExpression = PSyn.pLHSExpression PExpr.pExpression

lhsVar :: Text -> Surface.LHSExpression ()
lhsVar name =
  Surface.LHSExpression
    { lhsExpression = Syn.LHSEVar C.SimpleVarIdentifier{name, ann = ()}
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
          C.SimpleVarIdentifier
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
          ( C.AnyVarIdentifier
              { qualifier = mempty
              , qualifierAnn = Nothing
              , identifier = C.SimpleVarIdentifier{name, ann = ()}
              , identifierAnn = ()
              , ann = ()
              }
          )
    , ann = ()
    }

spec :: Spec
spec = describe "LHSExpression parser (minimal subset)" do
  it "parses variable x" do
    testParser "x" pLHSExpression (Just (lhsVar "x"))

  it "parses index a[b]" do
    let expected = lhsIndex (lhsVar "a") (exprVar "b")
    testParser "a[b]" pLHSExpression (Just expected)

  it "parses dot access a.b" do
    let expected = lhsDot (lhsVar "a") "b"
    testParser "a.b" pLHSExpression (Just expected)

  it "parses multiple dot accesses a.b.c" do
    let expected = lhsDot (lhsDot (lhsVar "a") "b") "c"
    testParser "a.b.c" pLHSExpression (Just expected)

  it "parses deref *a" do
    let expected = lhsDeref (exprVar "a")
    testParser "*a" pLHSExpression (Just expected)
