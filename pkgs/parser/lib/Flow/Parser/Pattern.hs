module Flow.Parser.Pattern where

import "base" Data.Maybe (fromJust)
import "base" Data.Functor ((<&>))
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, anyTypeIdentifier, simpleTypeIdentifier, simpleVarIdentifier, single, token)
import Flow.Parser.Literal (literal)

pPattern ::
  forall pat ty.
  Parser (pat Lexer.SourceRegion, Lexer.SourceRegion) ->
  Parser (Surface.PatternF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pPattern pPat = do
  Megaparsec.choice
    [ pPatternSimple pPat <&> \(simple, ann) -> (Surface.PatternSimpleF simple ann, ann)
    ]

pPatternSimple ::
  forall pat ty.
  Parser (pat Lexer.SourceRegion, Lexer.SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pPatternSimple pPat =
  Megaparsec.choice
    [ pWildcard
    , pLiteral
    , pVar
    , pTuple pPat
    , pCons pPat
    ]

pWildcard :: Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure (Surface.PatternSimpleWildcardF, tok.payload)

pLiteral :: Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pLiteral = do
  (lit, ann) <- literal
  pure (Surface.PatternSimpleLiteralF lit, ann)

pVar :: Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pVar = do
  name <- simpleVarIdentifier
  pure (Surface.PatternSimpleVarF name, name.ann)

pTuple ::
  Parser (pat Lexer.SourceRegion, Lexer.SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pTuple p = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  items <- Megaparsec.sepEndBy1 p (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( Surface.PatternSimpleTupleF (fromJust $ NonEmptyVector.fromList (fmap fst items))
    , Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
    )

pCons ::
  forall pat ty.
  Parser (pat Lexer.SourceRegion, Lexer.SourceRegion) ->
  Parser (Surface.PatternSimpleF pat ty Lexer.SourceRegion, Lexer.SourceRegion)
pCons pPat = do
  consName <- anyTypeIdentifier
  params <- Megaparsec.optional do
    tokS <- single (Lexer.Punctuation Lexer.LessThan)
    items <- Megaparsec.sepEndBy1 simpleTypeIdentifier (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
    pure
      ( Vector.fromList items
      , Lexer.SourceRegion
          { start = tokS.payload.start
          , end = tokE.payload.end
          }
      )
  fields <- Megaparsec.optional pFields
  let end = case fields of
        Just (_, ann) -> ann.end
        Nothing -> case params of
          Just (_, ann) -> ann.end
          Nothing -> consName.ann.end
  pure
    ( Surface.PatternSimpleConsF
        ( Surface.ConstructorAppF
            { name = consName
            , params = params
            , fields = fields
            , ann = Lexer.SourceRegion{start = consName.ann.start, end}
            }
        )
    , Lexer.SourceRegion{start = consName.ann.start, end}
    )
 where
  pFields = do
    Megaparsec.choice [pFieldsTuple, pFieldsNamed]

  pFieldsTuple = do
    tokS <- single (Lexer.Punctuation Lexer.LeftParen)
    items <- Megaparsec.sepEndBy1 pPat (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    pure
      ( Surface.FieldsTuple (Vector.fromList items)
      , Lexer.SourceRegion
          { start = tokS.payload.start
          , end = tokE.payload.end
          }
      )

  pFieldsNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    items <- Megaparsec.sepEndBy1 pFieldNamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.FieldsNamed (Vector.fromList items)
      , Lexer.SourceRegion
          { start = tokS.payload.start
          , end = tokE.payload.end
          }
      )
   where
    pFieldNamed :: Parser (Surface.SimpleVarIdentifier Lexer.SourceRegion, pat Lexer.SourceRegion, Lexer.SourceRegion)
    pFieldNamed = do
      name <- simpleVarIdentifier
      tok <- single (Lexer.Punctuation Lexer.Assign)
      value <- pPat
      pure (name, fst value, Lexer.SourceRegion{start = tok.payload.start, end = tok.payload.end})
