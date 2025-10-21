module Flow.Parser.Syntax where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Data.Set qualified as Set
import Flow.AST.Surface (Expression (..), LHSExpression (..), Pattern (..), PatternSimple (..), Type (..))
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

codeBlock ::
  Parser (LHSExpression SourceRegion) ->
  Parser (PatternSimple SourceRegion) ->
  Parser (Pattern SourceRegion) ->
  Parser (Type SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (CodeBlockF LHSExpression PatternSimple Pattern Type Expression SourceRegion)
codeBlock lhsExpr simPat pat ty expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <- Megaparsec.many (statement lhsExpr simPat pat ty expr)
  result <- Megaparsec.optional expr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
  pure $ CodeBlock{statements = Vector.fromList statements, result = result, ann = ann}

statement ::
  Parser (LHSExpression SourceRegion) ->
  Parser (PatternSimple SourceRegion) ->
  Parser (Pattern SourceRegion) ->
  Parser (Type SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (StatementF LHSExpression PatternSimple Pattern Type Expression SourceRegion)
statement lhsExpr simPat pat ty expr = do
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
    lhs <- simPat
    lhsType <- Megaparsec.optional do
      colonTok <- single (Lexer.Punctuation Lexer.Colon)
      ty' <- ty
      pure (ty', colonTok.region)
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = letTok.region.start, end = semicolonTok.region.end}
    pure $
      SLetF $
        LetDefinitionF
          { lhs = lhs
          , lhsAnn = lhs.ann
          , lhsType = lhsType
          , rhs = rhs
          , rhsAnn = rhs.ann
          , ann = ann
          }

  assignStatement = do
    lhs <- lhsExpr
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = lhs.ann.start, end = semicolonTok.region.end}
    pure $
      SAssignF $
        AssignStatementF
          { lhs = lhs
          , lhsAnn = lhs.ann
          , rhs = rhs
          , rhsAnn = rhs.ann
          , ann = ann
          }

  returnStatement = do
    returnTok <- single (Lexer.Keyword Lexer.Return)
    expr' <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = returnTok.region.start, end = semicolonTok.region.end}
    pure $ SReturnF expr' ann

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
    expr' <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = expr'.ann.start, end = semicolonTok.region.end}
    pure $ SExpressionF expr' ann

  matchStatement = do
    expr' <- matchExpression pat expr
    pure $ SMatchF expr' expr'.ann

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
pLHSExpression :: Parser (LHSExpression SourceRegion)
pLHSExpression = fail "Flow.Parser.Syntax.pLHSExpression: not implemented"

matchExpression ::
  Parser (Pattern SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (MatchExpression Pattern Expression SourceRegion)
matchExpression pat expr = do
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
    MatchExpression
      { value = value
      , valueAnn = value.ann
      , arms = NonEmptyVector.snocV (Vector.fromList arms) lastArm
      , armsAnn = armsAnn
      , ann = ann
      }
 where
  matchArm = do
    pattern' <- pat
    guard <- Megaparsec.optional do
      guardTok <- single (Lexer.Keyword Lexer.If)
      guardExpr <- expr
      pure (guardExpr, guardTok.region{Lexer.end = guardExpr.ann.end})
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    expr' <- expr
    pure $
      MatchArm
        { pattern = pattern'
        , patternAnn = pattern'.ann
        , guard = guard
        , expression = expr'
        , expressionAnn = expr'.ann
        , ann = pattern'.ann
        }

ifExpression ::
  Parser (Expression SourceRegion) ->
  Parser (IfExpression Expression SourceRegion)
ifExpression expr = do
  ifTok <- single (Lexer.Keyword Lexer.If)
  condition <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  then_ <- expr
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
    IfExpression
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
  Parser (LoopExpression Expression SourceRegion)
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
  pure $ LoopExpression{label = label, body = body, bodyAnn = body.ann, ann = ann}

whileExpression ::
  Parser (Expression SourceRegion) ->
  Parser (WhileExpression Expression SourceRegion)
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
  pure $ WhileExpression{label = label, condition = condition, conditionAnn = condition.ann, body = body, bodyAnn = body.ann, ann = ann}

forExpression ::
  Parser (Pattern SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (ForExpression Pattern Expression SourceRegion)
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
    ForExpression
      { label = label
      , pattern = pattern
      , patternAnn = pattern.ann
      , iterable = iterable
      , iterableAnn = iterable.ann
      , body = body
      , bodyAnn = body.ann
      , ann = ann
      }
