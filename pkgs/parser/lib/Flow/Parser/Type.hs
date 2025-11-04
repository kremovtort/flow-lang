{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Type where

import "base" Data.Maybe (fromJust)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Type (FnEffectsF (..))
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, SourceRegion (..), scopeIdentifier, simpleVarIdentifier, single, token)
import Flow.Parser.Constraint (anyTypeIdentifier, pBindersApp, pBindersWConstraints, pWhereBlockNested)

pType :: Parser (Surface.Type SourceRegion)
pType = do
  Megaparsec.choice
    [ pNotSuffixable'
    , do
        ty <- pSuffixable'
        Megaparsec.choice [pAppSuffix' ty, pTyEqualsSuffix' ty, pure ty]
    ]
 where
  pSuffixable' =
    Megaparsec.choice
      [ pIdentifier'
      , pParens'
      ]

  pNotSuffixable' =
    Megaparsec.choice
      [ pBuiltin'
      , pTuple'
      , pRefApp'
      , pRef'
      , pFn'
      , pEffectRow'
      , pForall'
      ]

  pBuiltin' = do
    (b, ann) <- pBuiltin
    pure Surface.Type{ty = Surface.TyBuiltinF b, ann = ann}

  pIdentifier' = do
    i <- pIdentifier pType
    pure Surface.Type{ty = Surface.TyIdentifierF i, ann = i.ann}

  pParens' = do
    (ty, ann) <- pParens pType
    pure Surface.Type{ty = Surface.TyParensF ty, ann = ann}

  pTuple' = do
    (args, ann) <- pTuple pType
    pure Surface.Type{ty = Surface.TyTupleF args, ann = ann}

  pRef' = do
    r <- pRef
    pure Surface.Type{ty = Surface.TyRefF r, ann = r.ann}

  pRefApp' = do
    r <- pRef
    inner <- pType
    pure Surface.Type{ty = Surface.TyRefAppF r inner, ann = inner.ann}

  pFn' = do
    fn <- pFn pType
    pure Surface.Type{ty = Surface.TyFnF fn, ann = fn.ann}

  pEffectRow' = do
    row <- pEffectRow pType
    pure Surface.Type{ty = Surface.TyEffectRowF row, ann = row.ann}

  pForall' = do
    forall' <- pForall pType
    pure Surface.Type{ty = Surface.TyForallF forall', ann = forall'.ann}

  pAppSuffix' ty = do
    app <- pAppSuffix pType ty
    pure Surface.Type{ty = Surface.TyAppF app, ann = app.ann}

  pTyEqualsSuffix' ty = do
    (left, right) <- pTyEqualsSuffix pType ty
    pure
      Surface.Type
        { ty = Surface.TyEquals left right
        , ann = SourceRegion{start = left.ann.start, end = right.ann.end}
        }

pBuiltin :: Parser (Surface.Builtin, SourceRegion)
pBuiltin = do
  tok <- token (Set.singleton $ Megaparsec.Label "builtin type") \case
    Lexer.Punctuation Lexer.LeftRightParen -> Just Surface.BuiltinUnit
    Lexer.Punctuation Lexer.Not -> Just Surface.BuiltinNever
    Lexer.Identifier "bool" -> Just Surface.BuiltinBool
    Lexer.Identifier "i8" -> Just Surface.BuiltinI8
    Lexer.Identifier "i16" -> Just Surface.BuiltinI16
    Lexer.Identifier "i32" -> Just Surface.BuiltinI32
    Lexer.Identifier "i64" -> Just Surface.BuiltinI64
    Lexer.Identifier "i128" -> Just Surface.BuiltinI128
    Lexer.Identifier "isize" -> Just Surface.BuiltinISize
    Lexer.Identifier "u8" -> Just Surface.BuiltinU8
    Lexer.Identifier "u16" -> Just Surface.BuiltinU16
    Lexer.Identifier "u32" -> Just Surface.BuiltinU32
    Lexer.Identifier "u64" -> Just Surface.BuiltinU64
    Lexer.Identifier "u128" -> Just Surface.BuiltinU128
    Lexer.Identifier "usize" -> Just Surface.BuiltinUSize
    Lexer.Identifier "f32" -> Just Surface.BuiltinF32
    Lexer.Identifier "f64" -> Just Surface.BuiltinF64
    Lexer.Identifier "f128" -> Just Surface.BuiltinF128
    Lexer.Identifier "byte" -> Just Surface.BuiltinByte
    Lexer.Identifier "bytestring" -> Just Surface.BuiltinByteString
    Lexer.Identifier "char" -> Just Surface.BuiltinChar
    Lexer.Identifier "string" -> Just Surface.BuiltinString
    _ -> Nothing
  pure (tok.value, tok.region)

pIdentifier ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.AnyTypeIdentifier ty SourceRegion)
pIdentifier = anyTypeIdentifier

pParens ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (ty SourceRegion, SourceRegion)
pParens pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  ty <- pTy
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure (ty, SourceRegion{start = tokS.region.start, end = tokE.region.end})

pTuple ::
  Parser (Surface.Type SourceRegion) ->
  Parser (NonEmptyVector (Surface.Type SourceRegion), SourceRegion)
pTuple pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( fromJust $ NonEmptyVector.fromList args
    , SourceRegion{start = tokS.region.start, end = tokE.region.end}
    )

pRef :: Parser (Surface.RefF SourceRegion)
pRef = do
  tokS <- single (Lexer.Punctuation Lexer.Ampersand)
  scope <- Megaparsec.optional $ do
    tok <-
      token (Set.singleton $ Megaparsec.Label "scope identifier") \case
        Lexer.RefScope i -> Just i
        _ -> Nothing
    pure $ Surface.ScopeIdentifier{name = tok.value, ann = tok.region}
  mut <- Megaparsec.optional $ do
    tok <- single (Lexer.Keyword Lexer.Mut)
    pure SourceRegion{start = tok.region.start, end = tok.region.end}
  pure $
    Surface.RefF
      { scope = scope
      , mutability = mut
      , ann =
          SourceRegion
            { start = tokS.region.start
            , end = case mut of
                Just mut' -> mut'.end
                Nothing -> case scope of
                  Just scope' -> scope'.ann.end
                  Nothing -> tokS.region.end
            }
      }

pAppSuffix ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  ty SourceRegion ->
  Parser (Surface.AppF ty SourceRegion)
pAppSuffix pTy ty = do
  args <- pBindersApp pTy
  pure
    Surface.AppF
      { head = ty
      , args = args
      , ann = SourceRegion{start = ty.ann.start, end = args.ann.end}
      }

pFn ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.FnF ty SourceRegion)
pFn pTy = do
  tokS <- single (Lexer.Keyword Lexer.Fn)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy pTy (single (Lexer.Punctuation Lexer.Comma))
  argsClose <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional (pFnEffectsResult pTy)
  pure $
    Surface.FnF
      { args = Vector.fromList args
      , effectsResult = effectsResult
      , ann =
          SourceRegion
            { start = tokS.region.start
            , end = case effectsResult of
                Just effectsResult' -> effectsResult'.ann.end
                Nothing -> argsClose.region.end
            }
      }

pFnEffectsResult ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.FnEffectsResultF ty SourceRegion)
pFnEffectsResult pTy = do
  tokArrow <- single (Lexer.Punctuation Lexer.Arrow)
  effects <- Megaparsec.optional do
    Megaparsec.choice
      [ FnEffectsRowF <$> pFnEffectRow pTy
      , FnEffectsTypeF <$> do
          _ <- single (Lexer.Punctuation Lexer.At)
          pTy
      ]
  result <- pTy
  pure
    Surface.FnEffectsResultF
      { effects = effects
      , result = result
      , ann = SourceRegion{start = tokArrow.region.start, end = result.ann.end}
      }

pFnEffectRow ::
  forall ty.
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.FnEffectRowF ty SourceRegion)
pFnEffectRow pTy = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepEndBy1 effectRowElem (single (Lexer.Punctuation Lexer.Comma))
  (regions, effects', tailVars) <- collect effects [] [] []
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    Surface.FnEffectRowF
      { regions = Vector.fromList regions
      , effects = Vector.fromList effects'
      , tailVars = Vector.fromList tailVars
      , ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }
 where
  effectRowElem = do
    Megaparsec.choice
      [ Left . Left <$> scopeIdentifier
      , Left . Right <$> effectAtom
      , Right <$> tailVarElem
      ]

  collect [] scopesAcc effectsAcc tailVarsAcc =
    pure (reverse scopesAcc, reverse effectsAcc, reverse tailVarsAcc)
  collect (Left (Left scope) : rest) scopesAcc effectsAcc tailVarsAcc
    | not (null effectsAcc) || not (null tailVarsAcc) =
        fail "scope argument cannot follow effect argument in effect row"
    | otherwise =
        collect rest (scope : scopesAcc) effectsAcc tailVarsAcc
  collect (Left (Right effect) : rest) scopesAcc effectsAcc tailVarsAcc
    | not (null tailVarsAcc) =
        fail "effect argument cannot follow tail variable in effect row"
    | otherwise =
        collect rest scopesAcc (effect : effectsAcc) tailVarsAcc
  collect (Right tailVar : rest) scopesAcc effectsAcc tailVarsAcc =
    collect rest scopesAcc effectsAcc (tailVar : tailVarsAcc)

  tailVarElem :: Parser (ty SourceRegion, SourceRegion)
  tailVarElem = do
    tokDD <- single (Lexer.Punctuation Lexer.DotDot)
    ty <- pTy
    pure (ty, SourceRegion{start = tokDD.region.start, end = ty.ann.end})

  effectAtom :: Parser (Surface.FnEffectAtomF ty SourceRegion)
  effectAtom = do
    Megaparsec.choice
      [ eAtomNameType
      , eAtomType
      ]
   where
    eAtomNameType :: Parser (Surface.FnEffectAtomF ty SourceRegion)
    eAtomNameType = do
      name <- simpleVarIdentifier
      _ <- single (Lexer.Punctuation Lexer.Colon)
      Surface.FnEffectAtomNameTypeF name <$> pTy

    eAtomType :: Parser (Surface.FnEffectAtomF ty SourceRegion)
    eAtomType = Surface.FnEffectAtomTypeF <$> pTy

pEffectRow ::
  forall ty.
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.EffectRowF ty SourceRegion)
pEffectRow pTy = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepEndBy1 effectRowElem (single (Lexer.Punctuation Lexer.Comma))
  (regions, effects', tailVars) <- collect effects [] [] []
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $
    Surface.EffectRowF
      { regions = Vector.fromList regions
      , effects = Vector.fromList effects'
      , tailVars = Vector.fromList tailVars
      , ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }
 where
  effectRowElem = do
    Megaparsec.choice
      [ Left . Left <$> scopeIdentifier
      , Left . Right <$> pTy
      , Right <$> tailVarElem
      ]

  tailVarElem :: Parser (ty SourceRegion, SourceRegion)
  tailVarElem = do
    tokDD <- single (Lexer.Punctuation Lexer.DotDot)
    ty <- pTy
    pure (ty, SourceRegion{start = tokDD.region.start, end = ty.ann.end})

  collect [] scopesAcc effectsAcc tailVarsAcc =
    pure (reverse scopesAcc, reverse effectsAcc, reverse tailVarsAcc)
  collect (Left (Left scope) : rest) scopesAcc effectsAcc tailVarsAcc
    | not (null effectsAcc) || not (null tailVarsAcc) =
        fail "scope argument cannot follow effect argument in effect row"
    | otherwise =
        collect rest (scope : scopesAcc) effectsAcc tailVarsAcc
  collect (Left (Right effect) : rest) scopesAcc effectsAcc tailVarsAcc
    | not (null tailVarsAcc) =
        fail "effect argument cannot follow tail variable in effect row"
    | otherwise =
        collect rest scopesAcc (effect : effectsAcc) tailVarsAcc
  collect (Right tailVar : rest) scopesAcc effectsAcc tailVarsAcc =
    collect rest scopesAcc effectsAcc (tailVar : tailVarsAcc)

pForall ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.ForallF ty SourceRegion)
pForall pTy = do
  params <- pBindersWConstraints pTy
  result <- pTy
  whereBlock <- Megaparsec.optional (pWhereBlockNested pTy)
  pure
    Surface.ForallF
      { params
      , result
      , whereBlock
      , ann =
          SourceRegion
            { start = params.ann.start
            , end = case whereBlock of
                Just whereBlock' -> whereBlock'.ann.end
                Nothing -> result.ann.end
            }
      }

pTyEqualsSuffix ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  ty SourceRegion ->
  Parser (ty SourceRegion, ty SourceRegion)
pTyEqualsSuffix pTy left = do
  _ <- single (Lexer.Punctuation Lexer.Equal)
  right <- pTy
  pure (left, right)
