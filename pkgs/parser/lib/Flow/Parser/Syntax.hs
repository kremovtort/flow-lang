module Flow.Parser.Syntax where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Data.Set qualified as Set
import Flow.AST.Surface (
  Expression (..),
  LHSExpression (..),
  Pattern (..),
  PatternSimple (..),
  Statement (..),
  Type (..),
 )
import Flow.AST.Surface.Common (SimpleVarIdentifier (..))
import Flow.AST.Surface.Syntax
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  Parser,
  SourceRegion (..),
  WithPos (region),
  simpleVarIdentifier,
  single,
  token,
 )

pCodeBlock ::
  Parser (Statement SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (CodeBlockF Statement Expression SourceRegion)
pCodeBlock pStmt pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <- Megaparsec.many pStmt
  result <- Megaparsec.optional expr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
  pure $ CodeBlockF{statements = Vector.fromList statements, result = result, ann = ann}

statement ::
  Parser (Statement SourceRegion) ->
  Parser (LHSExpression SourceRegion) ->
  Parser (PatternSimple SourceRegion) ->
  Parser (Pattern SourceRegion) ->
  Parser (Type SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (StatementF Statement LHSExpression PatternSimple Pattern Type Expression SourceRegion)
statement pStmt pLhsExpr pSimPat pPat pTy pExpr = do
  Megaparsec.choice
    [ letStatement
    , assignStatement
    , returnStatement
    , continueStatement
    , breakStatement
    , expressionStatement
    , matchStatement
    , ifStatement
    , loopStatement
    , whileStatement
    , forStatement
    ]
 where
  letStatement = do
    letTok <- single (Lexer.Keyword Lexer.Let)
    lhs <- pSimPat
    lhsType <- Megaparsec.optional do
      colonTok <- single (Lexer.Punctuation Lexer.Colon)
      ty <- pTy
      pure (ty, colonTok.region)
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = letTok.region.start, end = semicolonTok.region.end}
    pure $
      SLetF $
        LetDefinitionF
          { lhs = lhs
          , lhsType = lhsType
          , rhs = rhs
          , ann = ann
          }

  assignStatement = do
    lhs <- pLhsExpr
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = lhs.ann.start, end = semicolonTok.region.end}
    pure $
      SAssignF $
        AssignStatementF
          { lhs = lhs
          , rhs = rhs
          , ann = ann
          }

  returnStatement = do
    returnTok <- single (Lexer.Keyword Lexer.Return)
    expr <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = returnTok.region.start, end = semicolonTok.region.end}
    pure $ SReturnF expr ann

  continueStatement = do
    continueTok <- single (Lexer.Keyword Lexer.Continue)
    label <- Megaparsec.optional simpleVarIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = continueTok.region.start, end = semicolonTok.region.end}
    pure $ SContinueF label ann

  breakStatement = do
    breakTok <- single (Lexer.Keyword Lexer.Break)
    label <- Megaparsec.optional simpleVarIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = breakTok.region.start, end = semicolonTok.region.end}
    pure $ SBreakF label ann

  expressionStatement = do
    expr <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = expr.ann.start, end = semicolonTok.region.end}
    pure $ SExpressionF expr ann

  matchStatement = do
    expr <- matchExpression pPat pExpr
    pure $ SMatchF expr expr.ann

  ifStatement = do
    expr' <- ifExpression expr
    pure $ SIfF expr' expr'.ann

  loopStatement = do
    expr' <- loopExpression expr
    pure $ SLoopF expr' expr'.ann

  whileStatement = do
    expr' <- whileExpression expr
    pure $ SWhileF expr' expr'.ann

  forStatement = do
    expr' <- forExpression pat expr
    pure $ SForF expr' expr'.ann

-- | LHSExpression parser (stub)
pLHSExpression :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion)
pLHSExpression expr = do
  base <- pLHSEAtom expr
  suffixes base
 where
  suffixes acc = do
    mSuffix <- Megaparsec.optional (Megaparsec.choice [pLHSEIndexSuffix expr, pLHSEDotAccessSuffix])
    case mSuffix of
      Just applySuffix -> suffixes (applySuffix acc)
      Nothing -> pure acc

-- | Parses an index suffix: "[expr]" and returns a function to extend LHS
pLHSEIndexSuffix :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion -> LHSExpression SourceRegion)
pLHSEIndexSuffix expr = do
  _ <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $ \acc ->
    LHSExpression
      { lhsExpression = LHSEIndex acc idxExpr
      , ann = SourceRegion{start = acc.ann.start, end = tokE.region.end}
      }

-- | Parses a dot-access suffix: ".field" and returns a function to extend LHS
pLHSEDotAccessSuffix :: Parser (LHSExpression SourceRegion -> LHSExpression SourceRegion)
pLHSEDotAccessSuffix = do
  _ <- single (Lexer.Punctuation Lexer.Dot)
  field' <- simpleVarIdentifier
  pure $ \acc ->
    LHSExpression
      { lhsExpression = LHSEDotAccess acc field'
      , ann = SourceRegion{start = acc.ann.start, end = field'.ann.end}
      }

pLHSEAtom :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion)
pLHSEAtom expr = do
  Megaparsec.choice
    [ pLHSEWildcard
    , pLHSEVar
    , pLHSEUnOp expr
    ]

pLHSEWildcard :: Parser (LHSExpression SourceRegion)
pLHSEWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure $ LHSExpression{lhsExpression = LHSEWildcard, ann = tok.region}

pLHSEVar :: Parser (LHSExpression SourceRegion)
pLHSEVar = do
  var <- simpleVarIdentifier
  pure $ LHSExpression{lhsExpression = LHSEVar var, ann = var.ann}

pLHSEUnOp :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion)
pLHSEUnOp pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.Star)
  expr <- pExpr
  pure $ LHSExpression{lhsExpression = LHSEUnOp (LHSUnOpExpressionDeref expr), ann = SourceRegion{start = tokS.region.start, end = expr.ann.end}}

matchExpression ::
  Parser (Pattern SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (MatchExpressionF Pattern Expression SourceRegion)
matchExpression pPat pExpr = do
  matchTok <- single (Lexer.Keyword Lexer.Match)
  value <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  arms <- Megaparsec.many do
    arm <- matchArm
    commaTok <- single (Lexer.Punctuation Lexer.Comma)
    pure (arm, SourceRegion{start = arm.ann.start, end = commaTok.region.end})
  lastArm <- do
    arm <- matchArm
    mCommaTok <- Megaparsec.optional (single (Lexer.Punctuation Lexer.Comma))
    let end = case mCommaTok of
          Just commaTok -> commaTok.region.end
          Nothing -> arm.ann.end
    pure (arm, SourceRegion{start = arm.ann.start, end = end})
  let armsAnn = case arms of
        [] -> snd lastArm
        (_, startAnn) : _ -> SourceRegion{start = startAnn.start, end = (snd lastArm).end}
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceRegion{start = matchTok.region.start, end = rightBraceTok.region.end}
  pure $
    MatchExpressionF
      { value = value
      , arms = NonEmptyVector.snocV (Vector.fromList arms) lastArm
      , ann = ann
      }
 where
  matchArm = do
    pattern' <- pPat
    guard <- Megaparsec.optional do
      guardTok <- single (Lexer.Keyword Lexer.If)
      guardExpr <- expr
      pure (guardExpr, guardTok.region{Lexer.end = guardExpr.ann.end})
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    expr' <- expr
    pure $
      MatchArmF
        { pattern = pattern'
        , guard = guard
        , expression = expr'
        , ann = pattern'.ann
        }

ifExpression ::
  Parser (Statement SourceRegion) ->
  Parser (Pattern SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (IfExpressionF Expression SourceRegion)
ifExpression pStmt pPat pExpr = do
  ifTok <- single (Lexer.Keyword Lexer.If)
  condition <- pExpr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  then_ <- pExpr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  elseIfs <- Megaparsec.many elseIf
  else_ <- Megaparsec.optional do
    elseTok <- single (Lexer.Keyword Lexer.Else)
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    else' <- expr
    _ <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (else', SourceRegion{start = elseTok.region.start, end = rightBraceTok.region.end})
  let ann =
        SourceRegion
          { start = ifTok.region.start
          , end = case else_ of
              Just (_, ann') -> ann'.end
              Nothing -> rightBraceTok.region.end
          }
  pure $
    IfExpressionF
      { condition = condition
      , conditionAnn = condition.ann
      , then_ = then_
      , thenAnn = then_.ann
      , elseIfs = Vector.fromList elseIfs
      , else_ = else_
      , ann = ann
      }
 where
  elseIf = do
    _ <- single (Lexer.Keyword Lexer.Else)
    ifTok <- single (Lexer.Keyword Lexer.If)
    condition <- expr
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    then_ <- expr
    rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( condition
      , then_
      , SourceRegion
          { start = ifTok.region.start
          , end = rightBraceTok.region.end
          }
      )

loopExpression ::
  Parser (Expression SourceRegion) ->
  Parser (LoopExpressionF Expression SourceRegion)
loopExpression expr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier
          { name = labelTok.value
          , ann = labelTok.region
          }
      , SourceRegion
          { start = labelTok.region.start
          , end = labelTok.region.end
          }
      )
  loopTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.Loop)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> loopTok.region.start
          , end = rightBraceTok.region.end
          }
  pure $
    LoopExpressionF
      { label = label
      , body = body
      , ann = ann
      }

whileExpression ::
  Parser (Expression SourceRegion) ->
  Parser (WhileExpressionF Expression SourceRegion)
whileExpression expr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier labelTok.value labelTok.region
      , SourceRegion
          { start = labelTok.region.start
          , end = labelTok.region.end
          }
      )
  whileTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.While)
  condition <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> whileTok.region.start
          , end = rightBraceTok.region.end
          }
  pure $
    WhileExpressionF
      { label = label
      , condition = condition
      , body = body
      , ann = ann
      }

forExpression ::
  Parser (Pattern SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (ForExpressionF Pattern Expression SourceRegion)
forExpression pat expr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier labelTok.value labelTok.region
      , SourceRegion
          { start = labelTok.region.start
          , end = labelTok.region.end
          }
      )
  forTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.For)
  pattern <- pat
  _ <- single (Lexer.Keyword Lexer.In)
  iterable <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> forTok.region.start
          , end = rightBraceTok.region.end
          }
  pure $
    ForExpressionF
      { label = label
      , pattern = pattern
      , iterable = iterable
      , body = body
      , ann = ann
      }
