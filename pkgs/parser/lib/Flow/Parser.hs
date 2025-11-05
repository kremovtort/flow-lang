{-# LANGUAGE DuplicateRecordFields #-}

module Flow.Parser where


import Flow.AST.Surface (
  Expression (..),
  LHSExpression (..),
  Pattern (..),
  PatternSimple (..),
  Statement (..),
  Type (..),
  Mod (..), ModDefinitionBody,
 )
import Flow.Lexer (SourceRegion)
import Flow.Parser.Common (Parser)
import Flow.Parser.Expr qualified as PExpr
import Flow.Parser.Pattern qualified as PPat
import Flow.Parser.Syntax qualified as PSyn
import Flow.Parser.Type qualified as PType
import Flow.Parser.Module qualified as PMod

pExpression :: Parser (Expression SourceRegion)
pExpression = PExpr.pExpression pStatement pPatternSimple pPattern pType pExpression

pStatement :: Parser (Statement SourceRegion)
pStatement = do
  (stmt, ann) <- PSyn.pStatement pStatement pLhsExpression pPatternSimple pPattern pType pExpression
  pure $ Statement stmt ann

pLhsExpression :: Parser (LHSExpression SourceRegion)
pLhsExpression = PSyn.pLHSExpression pExpression

pPatternSimple :: Parser (PatternSimple SourceRegion)
pPatternSimple = do
  (simPat, ann) <- PPat.pPatternSimple pPatternSimple pType
  pure
    PatternSimple
      { patternSimple = simPat
      , ann
      }

pPattern :: Parser (Pattern SourceRegion)
pPattern = do
  (pattern, ann) <- PPat.pPattern pPattern pType
  pure
    Pattern
      { pattern
      , ann
      }

pType :: Parser (Type SourceRegion)
pType = PType.pType

pMod :: Parser (Mod SourceRegion)
pMod = do
  (mod', ann) <- PMod.pMod pMod pStatement pPatternSimple pType pExpression
  pure $ Mod mod' ann

pModDefinitionBody :: Parser (ModDefinitionBody SourceRegion)
pModDefinitionBody = PMod.pModDefinitionBody pMod pStatement pPatternSimple pType pExpression
