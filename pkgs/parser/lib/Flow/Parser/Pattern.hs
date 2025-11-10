module Flow.Parser.Pattern where

import "base" Data.Functor ((<&>))
import "base" Data.Maybe (fromJust)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector

import Data.Bifunctor qualified as Bifunctor
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, SourceSpan (..), pSimpleVarIdentifier, single)
import Flow.Parser.Constraint (anyTypeIdentifier, pBindersWoConstraints)
import Flow.Parser.Literal (literal)

pPattern ::
  forall pat ty.
  (HasAnn pat SourceSpan, HasAnn ty SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (Surface.PatternF pat ty SourceSpan, SourceSpan)
pPattern pPat pTy = Megaparsec.label "pattern" do
  Megaparsec.choice
    [ Bifunctor.first Surface.PatSimpleF <$> pPatternSimple pPat pTy
    , pLiteral
    , pOr pPat pTy
    ]

pPatternSimple ::
  forall pat ty.
  (HasAnn pat SourceSpan, HasAnn ty SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (Surface.PatternSimpleF pat ty SourceSpan, SourceSpan)
pPatternSimple pPat pTy =
  Megaparsec.choice
    [ pWildcard
    , pVar <&> \var -> (Surface.PatSimVarF var, var.ann)
    , pTuple pPat
    , pCons pPat pTy <&> \cons ->
        (Surface.PatSimConstructorAppF cons, cons.ann)
    ]

pWildcard :: Parser (Surface.PatternSimpleF pat ty SourceSpan, SourceSpan)
pWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure (Surface.PatSimWildcardF, tok.span)

pLiteral :: Parser (Surface.PatternF pat ty SourceSpan, SourceSpan)
pLiteral = do
  (lit, ann) <- literal
  pure (Surface.PatLiteralF lit, ann)

pVar :: Parser (Surface.PatternVariableF pat ty SourceSpan)
pVar = Megaparsec.label "pattern variable" do
  ref <- Megaparsec.optional (single (Lexer.Keyword Lexer.Ref))
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- pSimpleVarIdentifier
  pure
    Surface.PatternVariableF
      { ref = (.span) <$> ref
      , mut = (.span) <$> mut
      , name = name
      , ann =
          SourceSpan
            { start = case mut of
                Just mut' -> mut'.span.start
                Nothing -> name.ann.start
            , end = name.ann.end
            }
      }

pTuple ::
  Parser (pat SourceSpan) ->
  Parser (Surface.PatternSimpleF pat ty SourceSpan, SourceSpan)
pTuple p = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  items <- Megaparsec.sepEndBy1 p (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( Surface.PatSimTupleF (fromJust $ NonEmptyVector.fromList items)
    , SourceSpan{start = tokS.span.start, end = tokE.span.end}
    )

pCons ::
  forall pat ty.
  (HasAnn pat SourceSpan, HasAnn ty SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (Surface.PatternConsturctorAppF pat ty SourceSpan)
pCons pPat pTy = do
  consName <- anyTypeIdentifier pTy
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  fields <- Megaparsec.optional pFields
  let end = case fields of
        Just (_, ann) -> ann.end
        Nothing -> case typeParams of
          Just params -> params.ann.end
          Nothing -> consName.ann.end
  pure
    Surface.PatternConsturctorAppF
      { name = consName
      , typeParams = typeParams
      , fields = fields
      , ann = Lexer.SourceSpan{start = consName.ann.start, end}
      }
 where
  pFields = do
    Megaparsec.choice
      [ Bifunctor.first Surface.PatFldsUnnamedF <$> pFieldsUnnamed
      , Bifunctor.first Surface.PatFldsNamedF <$> pFieldsNamed
      ]

  pFieldsUnnamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftParen)
    fields <- Megaparsec.sepEndBy1 pFieldUnnamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    pure
      ( fromJust $ NonEmptyVector.fromList fields
      , SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )

  pFieldUnnamed = do
    value <- pPat
    optional <- Megaparsec.optional (single (Lexer.Punctuation Lexer.Question))
    pure
      Surface.PatternFieldUnnamedF
        { value
        , optional = (.span) <$> optional
        , ann =
            SourceSpan
              { start = value.ann.start
              , end = case optional of
                  Just optional' -> optional'.span.end
                  Nothing -> value.ann.end
              }
        }

  pFieldsNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    fields <- Megaparsec.sepEndBy1 pFieldNamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( fromJust $ NonEmptyVector.fromList fields
      , SourceSpan
          { start = tokS.span.start
          , end = tokE.span.end
          }
      )

  pFieldNamed :: Parser (Surface.PatternFieldNamedF pat ty SourceSpan)
  pFieldNamed =
    Megaparsec.choice
      [ Surface.PatFldNmdValueF <$> Megaparsec.try pFieldNamedValue
      , Surface.PatFldNmdPunningF <$> pFieldNamedPunning
      ]

  pFieldNamedValue :: Parser (Surface.PatternFieldNamedValueF pat ty SourceSpan)
  pFieldNamedValue = do
    name <- pSimpleVarIdentifier
    _ <- single (Lexer.Punctuation Lexer.Assign)
    value <- pPat
    pure
      Surface.PatternFieldNamedValueF
        { name
        , value
        , ann = SourceSpan{start = name.ann.start, end = value.ann.end}
        }

  pFieldNamedPunning :: Parser (Surface.PatternFieldNamedPunningF pat ty SourceSpan)
  pFieldNamedPunning = do
    ref <- Megaparsec.optional (single (Lexer.Keyword Lexer.Ref))
    mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    name <- pSimpleVarIdentifier
    optional <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Question)
    pure
      Surface.PatternFieldNamedPunningF
        { ref = (.span) <$> ref
        , mut = (.span) <$> mut
        , name
        , optional = (.span) <$> optional
        , ann =
            SourceSpan
              { start = name.ann.start
              , end = case optional of
                  Just optional' -> optional'.span.end
                  Nothing -> name.ann.end
              }
        }

pOr ::
  (HasAnn pat SourceSpan, HasAnn ty SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (Surface.PatternF pat ty SourceSpan, SourceSpan)
pOr pPat pTy = do
  items <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 (pPatternSimple pPat pTy) (single (Lexer.Punctuation Lexer.Pipe))
  pure
    ( Surface.PatOrF (fmap fst items)
    , SourceSpan
        { start = (snd $ NonEmptyVector.head items).start
        , end = (snd $ NonEmptyVector.last items).end
        }
    )
