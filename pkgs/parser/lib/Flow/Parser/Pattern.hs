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
import Flow.Parser.Common (HasAnn, Parser, anyTypeIdentifier, simpleVarIdentifier, single)
import Flow.Parser.Constraint (pBindersWoConstraints)
import Flow.Parser.Literal (literal)

pPattern ::
  forall pat ty.
  (HasAnn pat Lexer.SourceRegion, HasAnn ty Lexer.SourceRegion) =>
  Parser (pat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.PatternF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pPattern pPat pTy = do
  Megaparsec.choice
    [ Bifunctor.first Surface.PatSimpleF <$> pPatternSimple pPat pTy
    ]

pPatternSimple ::
  forall pat ty.
  (HasAnn pat Lexer.SourceRegion, HasAnn ty Lexer.SourceRegion) =>
  Parser (pat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pPatternSimple pPat pTy =
  Megaparsec.choice
    [ pWildcard
    , pLiteral
    , pVar <&> \var -> (Surface.PatSimVarF var, var.ann)
    , pTuple pPat
    , pCons pPat pTy <&> \cons -> (Surface.PatSimConstructorAppF cons, cons.ann)
    ]

pWildcard :: Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure (Surface.PatSimWildcardF, tok.payload)

pLiteral :: Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pLiteral = do
  (lit, ann) <- literal
  pure (Surface.PatSimLiteralF lit, ann)

pVar :: Parser (Surface.PatternVariableF pat ty Lexer.SourceRegion)
pVar = do
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- simpleVarIdentifier
  pure
    Surface.PatternVariableF
      { mut = (.payload) <$> mut
      , name = name
      , ann =
          Lexer.SourceRegion
            { start = case mut of
                Just mut' -> mut'.payload.start
                Nothing -> name.ann.start
            , end = name.ann.end
            }
      }

pTuple ::
  Parser (pat Lexer.SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pTuple p = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  items <- Megaparsec.sepEndBy1 p (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( Surface.PatSimTupleF (fromJust $ NonEmptyVector.fromList items)
    , Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
    )

pCons ::
  forall pat ty.
  (HasAnn pat Lexer.SourceRegion, HasAnn ty Lexer.SourceRegion) =>
  Parser (pat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.PatternConsturctorAppF pat ty Lexer.SourceRegion)
pCons pPat pTy = do
  consName <- anyTypeIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  fields <- Megaparsec.optional pFields
  let end = case fields of
        Just (_, ann) -> ann.end
        Nothing -> case typeParams of
          Just params -> params.ann.end
          Nothing -> consName.ann.end
  pure
    ( Surface.PatternConsturctorAppF
        { name = consName
        , typeParams = typeParams
        , fields = fields
        , ann = Lexer.SourceRegion{start = consName.ann.start, end}
        }
    )
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
      , Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
      )

  pFieldUnnamed = do
    value <- pPat
    optional <- Megaparsec.optional (single (Lexer.Punctuation Lexer.Question))
    pure
      Surface.PatternFieldUnnamedF
        { value
        , optional = (.payload) <$> optional
        , ann =
            Lexer.SourceRegion
              { start = value.ann.start
              , end = case optional of
                  Just optional' -> optional'.payload.end
                  Nothing -> value.ann.end
              }
        }

  pFieldsNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    fields <- Megaparsec.sepEndBy1 pFieldNamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( fromJust $ NonEmptyVector.fromList fields
      , Lexer.SourceRegion
          { start = tokS.payload.start
          , end = tokE.payload.end
          }
      )

  pFieldNamed :: Parser (Surface.PatternFieldNamedF pat ty Lexer.SourceRegion)
  pFieldNamed =
    Megaparsec.choice
      [ Surface.PatFldNmdValueF <$> pFieldNamedValue
      , Surface.PatFldNmdPunningF <$> pFieldNamedPunning
      ]

  pFieldNamedValue :: Parser (Surface.PatternFieldNamedValueF pat ty Lexer.SourceRegion)
  pFieldNamedValue = do
    name <- simpleVarIdentifier
    optional <-
      Megaparsec.choice
        [ Nothing <$ single (Lexer.Punctuation Lexer.Assign)
        , Just . (.payload) <$> single (Lexer.Punctuation Lexer.QuestionAssign)
        ]
    value <- pPat
    pure
      Surface.PatternFieldNamedValueF
        { name
        , value
        , optional = optional
        , ann = Lexer.SourceRegion{start = name.ann.start, end = value.ann.end}
        }

  pFieldNamedPunning :: Parser (Surface.PatternFieldNamedPunningF pat ty Lexer.SourceRegion)
  pFieldNamedPunning = do
    mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    name <- simpleVarIdentifier
    optional <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Question)
    pure
      Surface.PatternFieldNamedPunningF
        { mut = (.payload) <$> mut
        , name
        , optional = (.payload) <$> optional
        , ann =
            Lexer.SourceRegion
              { start = name.ann.start
              , end = case optional of
                  Just optional' -> optional'.payload.end
                  Nothing -> name.ann.end
              }
        }
