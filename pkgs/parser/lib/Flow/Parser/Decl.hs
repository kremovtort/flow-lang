module Flow.Parser.Decl where

import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Data.Maybe (fromJust)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NonEmptyVector
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.Lexer (SourceRegion (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, pPub, simpleTypeIdentifier, simpleVarIdentifier, single)
import Flow.Parser.Constraint (pBindersWConstraints, pBindersWoConstraints, pWhereBlockNested)

pStruct ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.StructF ty SourceRegion)
pStruct pTy = do
  structTok <- single (Lexer.Keyword Lexer.Struct)
  name <- simpleTypeIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  (fields, fieldsAnn) <- pFieldsDecl pTy
  let ann = Lexer.SourceRegion{start = structTok.region.start, end = fieldsAnn.end}
  pure
    Surface.StructF
      { name
      , typeParams
      , fields
      , ann
      }

pEnum ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.EnumF ty SourceRegion)
pEnum pTy = do
  enumTok <- single (Lexer.Keyword Lexer.Enum)
  name <- simpleTypeIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  (variants, variantsAnn) <- pEnumVariants
  let ann = Lexer.SourceRegion{start = enumTok.region.start, end = variantsAnn.end}
  pure
    Surface.EnumF
      { name
      , typeParams
      , variants
      , ann
      }
 where
  pEnumVariants = do
    Megaparsec.choice
      [ pEnumVariantsSimple
      , pEnumVariantsGeneralized
      ]
  pEnumVariantsSimple = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    variants <- Megaparsec.sepEndBy1 pEnumVariant (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.EVariantsSimpleF $ fromJust $ NonEmptyVector.fromList variants
      , Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      )
   where
    pEnumVariant = do
      name <- simpleTypeIdentifier
      fields <- Megaparsec.optional (pFieldsDecl pTy)
      let ann =
            Lexer.SourceRegion
              { start = name.ann.start
              , end = case fields of
                  Just (_, ann') -> ann'.end
                  Nothing -> name.ann.end
              }
      pure
        Surface.EnumVariantF
          { name
          , fields = fst <$> fields
          , ann
          }

  pEnumVariantsGeneralized = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    variants <- Megaparsec.sepEndBy1 pEnumVariantGeneralized (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.EVariantsGeneralized $ fromJust $ NonEmptyVector.fromList variants
      , Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      )
   where
    pEnumVariantGeneralized = do
      name <- simpleTypeIdentifier
      typeParams <- Megaparsec.optional (pBindersWConstraints pTy)
      fields <- Megaparsec.optional (pFieldsDecl pTy)
      _ <- single (Lexer.Punctuation Lexer.Colon)
      result <- pTy
      whereBlock <- Megaparsec.optional (pWhereBlockNested pTy)
      let ann = Lexer.SourceRegion{start = name.ann.start, end = result.ann.end}
      pure
        Surface.EnumVariantGeneralizedF
          { name
          , typeParams = typeParams
          , fields = fst <$> fields
          , result
          , whereBlock
          , ann
          }

pFieldsDecl ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.FieldsDeclF ty SourceRegion, SourceRegion)
pFieldsDecl pTy = do
  Megaparsec.choice
    [ pFieldsDeclNamed
    , pFieldsDeclTuple
    ]
 where
  pFieldsDeclNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    fields <- Megaparsec.sepEndBy pFieldDecl (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.FieldsDeclNamedF $ Vector.fromList fields
      , Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      )

  pFieldDecl = do
    pub <- Megaparsec.optional pPub
    name <- simpleVarIdentifier
    _ <- single (Lexer.Punctuation Lexer.Colon)
    type_ <- pTy
    pure
      Surface.FieldDeclF
        { pub = fmap fst pub
        , name = name
        , type_ = type_
        , ann =
            Lexer.SourceRegion
              { start = case pub of
                  Just (_, ann) -> ann.start
                  Nothing -> name.ann.start
              , end = type_.ann.end
              }
        }

  pFieldsDeclTuple = do
    tokS <- single (Lexer.Punctuation Lexer.LeftParen)
    fields <- Megaparsec.sepEndBy pTy (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    pure
      ( Surface.FieldsDeclTupleF $ Vector.fromList fields
      , Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      )
