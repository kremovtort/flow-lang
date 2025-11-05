module Flow.Parser.Decl where

import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Data.Maybe (fromJust)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NonEmptyVector
import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.Lexer (SourceRegion (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (
  pFnDeclaration,
  pFnDefinition,
  pFnInfixDeclaration,
  pFnInfixDefinition,
  pOpDeclaration,
  pOpDefinition,
  pOpInfixDeclaration,
  pOpInfixDefinition,
 )
import Flow.Parser.Common (HasAnn, Parser, pPub, simpleTypeIdentifier, simpleVarIdentifier, single)
import Flow.Parser.Constraint (pBindersWConstraints, pBindersWoConstraints, pKindTreeRoot, pWhereBlockHead, pWhereBlockNested)

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

pTrait ::
  (HasAnn stmt SourceRegion, HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (Surface.TraitF stmt ty expr SourceRegion)
pTrait pStmt pTy pExpr = do
  tokS <- single (Lexer.Keyword Lexer.Trait)
  name <- simpleTypeIdentifier
  typeParams <- pBindersWoConstraints pTy
  superTraits <-
    fmap (fromJust . NonEmptyVector.fromList) <$> Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.FatArrow)
      Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  traitBody <- Vector.fromList <$> Megaparsec.many pTraitItem
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure
    Surface.TraitF
      { name
      , typeParams
      , superTraits
      , traitBody
      , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }
 where
  pTraitItem = do
    pub <- Megaparsec.optional pPub
    (item, itemAnn) <- pTraitItemVariant
    pure
      Surface.TraitItemF
        { pub = fmap fst pub
        , item
        , ann =
            Lexer.SourceRegion
              { start = case pub of
                  Just (_, ann') -> ann'.start
                  Nothing -> itemAnn.start
              , end = itemAnn.end
              }
        }

  pTraitItemVariant =
    Megaparsec.choice
      [ withRegion Surface.TItemVarTypeDeclarationF <$> pTypeDeclaration pTy
      , withRegion Surface.TItemVarLetDeclarationF <$> pLetDeclaration pTy
      , withRegion Surface.TItemVarFnDeclarationF <$> Megaparsec.try (pFnDeclaration pTy)
      , withRegion Surface.TItemVarFnInfixDeclarationF <$> Megaparsec.try (pFnInfixDeclaration pTy)
      , withRegion Surface.TItemVarFnDefinitionF <$> Megaparsec.try (pFnDefinition pStmt pTy pExpr)
      , withRegion Surface.TItemVarFnInfixDefinitionF <$> Megaparsec.try (pFnInfixDefinition pStmt pTy pExpr)
      ]

  withRegion f item = (f item, item.ann)

pEffect ::
  (HasAnn stmt SourceRegion, HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (Surface.EffectF stmt ty expr SourceRegion)
pEffect pStmt pTy pExpr = do
  tokS <- single (Lexer.Keyword Lexer.Effect)
  name <- simpleTypeIdentifier
  typeParams <- Megaparsec.optional (pBindersWConstraints pTy)
  superEffects <-
    fmap (fromJust . NonEmptyVector.fromList) <$> Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.FatArrow)
      Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  effectBody <- Vector.fromList <$> Megaparsec.many pEffectItem
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure
    Surface.EffectF
      { name
      , typeParams
      , superEffects
      , whereBlock
      , effectBody
      , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }
 where
  pEffectItem = do
    pub <- Megaparsec.optional pPub
    (item, itemAnn) <- pEffectItemVariant
    pure
      Surface.EffectItemF
        { pub = fmap fst pub
        , item
        , ann = itemAnn
        }

  pEffectItemVariant =
    Megaparsec.choice
      [ withRegion Surface.EItemVarTypeDeclarationF <$> pTypeDeclaration pTy
      , withRegion Surface.EItemVarLetDeclarationF <$> pLetDeclaration pTy
      , withRegion Surface.EItemVarOpDeclarationF <$> Megaparsec.try (pOpDeclaration pTy)
      , withRegion Surface.EItemVarOpInfixDeclarationF <$> Megaparsec.try (pOpInfixDeclaration pTy)
      , withRegion Surface.EItemVarOpDefinitionF <$> Megaparsec.try (pOpDefinition pStmt pTy pExpr)
      , withRegion Surface.EItemVarOpInfixDefinitionF <$> Megaparsec.try (pOpInfixDefinition pStmt pTy pExpr)
      ]

  withRegion f item = (f item, item.ann)

pTypeDeclaration ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.TypeDeclarationF ty SourceRegion)
pTypeDeclaration pTy = do
  tokS <- single (Lexer.Keyword Lexer.Type)
  name <- simpleTypeIdentifier
  kindShort <- Megaparsec.optional (pKindTreeRoot pTy)
  type_ <- Megaparsec.optional pTy
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    Surface.TypeDeclarationF
      { name
      , kindShort
      , type_
      , ann =
          Lexer.SourceRegion
            { start = tokS.region.start
            , end = tokE.region.end
            }
      }

pLetDeclaration ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.LetDeclarationF ty SourceRegion)
pLetDeclaration pTy = do
  letTok <- single (Lexer.Keyword Lexer.Let)
  name <- simpleVarIdentifier
  _ <- single (Lexer.Punctuation Lexer.Colon)
  type_ <- pTy
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    Surface.LetDeclarationF
      { name
      , type_
      , ann = Lexer.SourceRegion{start = letTok.region.start, end = tokE.region.end}
      }
