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
import Flow.Parser.Common (HasAnn, Parser, SourceRegion (..), simpleVarIdentifier, single)
import Flow.Parser.Constraint (anyTypeIdentifier, pBindersWoConstraints)
import Flow.Parser.Literal (literal)

pPattern ::
  forall pat ty.
  (HasAnn pat SourceRegion, HasAnn ty SourceRegion) =>
  Parser (pat SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (Surface.PatternF pat ty SourceRegion, SourceRegion)
pPattern pPat pTy = do
  Megaparsec.choice
    [ Bifunctor.first Surface.PatSimpleF <$> pPatternSimple pPat pTy
    ]

pPatternSimple ::
  forall pat ty.
  (HasAnn pat SourceRegion, HasAnn ty SourceRegion) =>
  Parser (pat SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty SourceRegion, SourceRegion)
pPatternSimple pPat pTy =
  Megaparsec.choice
    [ pWildcard
    , pLiteral
    , pVar <&> \var -> (Surface.PatSimVarF var, var.ann)
    , pTuple pPat
    , pCons pPat pTy <&> \cons -> (Surface.PatSimConstructorAppF cons, cons.ann)
    ]

pWildcard :: Parser (Surface.PatternSimpleF pat ty SourceRegion, SourceRegion)
pWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure (Surface.PatSimWildcardF, tok.region)

pLiteral :: Parser (Surface.PatternSimpleF pat ty SourceRegion, SourceRegion)
pLiteral = do
  (lit, ann) <- literal
  pure (Surface.PatSimLiteralF lit, ann)

pVar :: Parser (Surface.PatternVariableF pat ty SourceRegion)
pVar = do
  ref <- Megaparsec.optional (single (Lexer.Keyword Lexer.Ref))
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- simpleVarIdentifier
  pure
    Surface.PatternVariableF
      { ref = (.region) <$> ref
      , mut = (.region) <$> mut
      , name = name
      , ann =
          SourceRegion
            { start = case mut of
                Just mut' -> mut'.region.start
                Nothing -> name.ann.start
            , end = name.ann.end
            }
      }

pTuple ::
  Parser (pat SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty SourceRegion, SourceRegion)
pTuple p = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  items <- Megaparsec.sepEndBy1 p (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( Surface.PatSimTupleF (fromJust $ NonEmptyVector.fromList items)
    , SourceRegion{start = tokS.region.start, end = tokE.region.end}
    )

pCons ::
  forall pat ty.
  (HasAnn pat SourceRegion, HasAnn ty SourceRegion) =>
  Parser (pat SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (Surface.PatternConsturctorAppF pat ty SourceRegion)
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
      , SourceRegion{start = tokS.region.start, end = tokE.region.end}
      )

  pFieldUnnamed = do
    value <- pPat
    optional <- Megaparsec.optional (single (Lexer.Punctuation Lexer.Question))
    pure
      Surface.PatternFieldUnnamedF
        { value
        , optional = (.region) <$> optional
        , ann =
            SourceRegion
              { start = value.ann.start
              , end = case optional of
                  Just optional' -> optional'.region.end
                  Nothing -> value.ann.end
              }
        }

  pFieldsNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    fields <- Megaparsec.sepEndBy1 pFieldNamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( fromJust $ NonEmptyVector.fromList fields
      , SourceRegion
          { start = tokS.region.start
          , end = tokE.region.end
          }
      )

  pFieldNamed :: Parser (Surface.PatternFieldNamedF pat ty SourceRegion)
  pFieldNamed =
    Megaparsec.choice
      [ Surface.PatFldNmdValueF <$> Megaparsec.try pFieldNamedValue
      , Surface.PatFldNmdPunningF <$> pFieldNamedPunning
      ]

  pFieldNamedValue :: Parser (Surface.PatternFieldNamedValueF pat ty SourceRegion)
  pFieldNamedValue = do
    name <- simpleVarIdentifier
    _ <- single (Lexer.Punctuation Lexer.Assign)
    value <- pPat
    pure
      Surface.PatternFieldNamedValueF
        { name
        , value
        , ann = SourceRegion{start = name.ann.start, end = value.ann.end}
        }

  pFieldNamedPunning :: Parser (Surface.PatternFieldNamedPunningF pat ty SourceRegion)
  pFieldNamedPunning = do
    ref <- Megaparsec.optional (single (Lexer.Keyword Lexer.Ref))
    mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    name <- simpleVarIdentifier
    optional <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Question)
    pure
      Surface.PatternFieldNamedPunningF
        { ref = (.region) <$> ref
        , mut = (.region) <$> mut
        , name
        , optional = (.region) <$> optional
        , ann =
            SourceRegion
              { start = name.ann.start
              , end = case optional of
                  Just optional' -> optional'.region.end
                  Nothing -> name.ann.end
              }
        }
