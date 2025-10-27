module Flow.Parser.Type where

import "base" Data.Functor ((<&>))
import "base" Data.Maybe (fromJust)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Type (TypeF (TyEffectRowF))
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, SourceRegion (..), scopeIdentifier, simpleVarIdentifier, single, token)
import Flow.Parser.Constraint (anyTypeIdentifier, pBindersApp)

-- Parse an atomic type (no application postfixed).
pTypeAtom :: Parser (Surface.Type SourceRegion)
pTypeAtom =
  Megaparsec.choice
    [ (\(b, ann) -> Surface.Type{ty = Surface.TyBuiltinF b ann, ann = ann}) <$> tyBuiltin
    , (\i -> Surface.Type{ty = Surface.TyIdentifierF i, ann = i.ann}) <$> tyIdentifier pType
    , (\(args, ann) -> Surface.Type{ty = Surface.TyTupleF args ann, ann = ann}) <$> tyTuple pType
    , (\r -> Surface.Type{ty = Surface.TyRefF r, ann = r.ann}) <$> tyRef
    , (\fn -> Surface.Type{ty = Surface.TyFnF fn, ann = fn.ann}) <$> tyFn pType
    , (\(row, ann) -> Surface.Type{ty = Surface.TyEffectRowF row, ann}) <$> tyEffectRow pType
    -- TODO: forall
    ]

-- Top-level type parser: parse an atom, then optionally parse application(s)
-- as postfix forms. This avoids left recursion by ensuring the head is
-- consumed before parsing any applications.
pType :: Parser (Surface.Type SourceRegion)
pType = do
  head' <- pTypeAtom
  case head'.ty of
    -- Reference application is a special postfix that consumes a single inner type.
    Surface.TyRefF _ -> do
      Megaparsec.choice
        [ do
            inner <- pType
            pure $
              Surface.Type
                { ty =
                    Surface.TyAppF
                      Surface.AppF
                        { head = head'
                        , args =
                            Surface.BindersF
                              { scopes = mempty
                              , types = Vector.singleton $ Surface.BinderAppF inner
                              , ann = inner.ann
                              }
                        , ann = inner.ann
                        }
                , ann = inner.ann
                }
        , pure head'
        ]
    _ -> do
      Megaparsec.choice
        [ do
            args <- pBindersApp pType
            pure $
              Surface.Type
                { ty =
                    Surface.TyAppF
                      Surface.AppF
                        { head = head'
                        , args = args
                        , ann = SourceRegion{start = head'.ann.start, end = args.ann.end}
                        }
                , ann = SourceRegion{start = head'.ann.start, end = args.ann.end}
                }
        , pure head'
        ]

tyBuiltin :: Parser (Surface.Builtin, SourceRegion)
tyBuiltin = do
  tok <- token (Set.singleton $ Megaparsec.Label "builtin type") \case
    Lexer.Punctuation Lexer.LeftRightParen -> Just Surface.BuiltinUnit
    Lexer.Punctuation Lexer.Not -> Just Surface.BuiltinNever
    Lexer.Identifier "bool" -> Just Surface.BuiltinBool
    Lexer.Identifier "i8" -> Just Surface.BuiltinI8
    Lexer.Identifier "i16" -> Just Surface.BuiltinI16
    Lexer.Identifier "i32" -> Just Surface.BuiltinI32
    Lexer.Identifier "i64" -> Just Surface.BuiltinI64
    Lexer.Identifier "u8" -> Just Surface.BuiltinU8
    Lexer.Identifier "u16" -> Just Surface.BuiltinU16
    Lexer.Identifier "u32" -> Just Surface.BuiltinU32
    Lexer.Identifier "u64" -> Just Surface.BuiltinU64
    Lexer.Identifier "f32" -> Just Surface.BuiltinF32
    Lexer.Identifier "f64" -> Just Surface.BuiltinF64
    Lexer.Identifier "f128" -> Just Surface.BuiltinF128
    Lexer.Identifier "byte" -> Just Surface.BuiltinByte
    Lexer.Identifier "byteString" -> Just Surface.BuiltinByteString
    Lexer.Identifier "char" -> Just Surface.BuiltinChar
    Lexer.Identifier "string" -> Just Surface.BuiltinString
    _ -> Nothing
  pure (tok.value, tok.region)

tyIdentifier ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (Surface.AnyTypeIdentifier ty SourceRegion)
tyIdentifier = anyTypeIdentifier

tyTuple ::
  Parser (Surface.Type SourceRegion) ->
  Parser (NonEmptyVector (Surface.Type SourceRegion), SourceRegion)
tyTuple ty = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy1 ty (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( fromJust $ NonEmptyVector.fromList args
    , SourceRegion{start = tokS.region.start, end = tokE.region.end}
    )

tyRef :: Parser (Surface.RefF SourceRegion)
tyRef = do
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

tyFn ::
  Parser (Surface.Type SourceRegion) ->
  Parser (Surface.FnF Surface.Type SourceRegion)
tyFn ty = do
  tokS <- single (Lexer.Keyword Lexer.Fn)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy ty (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.RightParen)
  tokE <- single (Lexer.Punctuation Lexer.Arrow)
  effects <- Megaparsec.optional do
    Megaparsec.choice
      [ do
          _ <- Megaparsec.lookAhead $ single (Lexer.Punctuation Lexer.AtLeftBracket)
          -- parse effect row
          (row, ann) <- tyEffectRow ty
          pure
            Surface.Type
              { ty = TyEffectRowF row
              , ann = ann
              }
      , do
          _ <- single (Lexer.Punctuation Lexer.At)
          ty
      ]
  result <- ty
  pure $
    Surface.FnF
      { args = Vector.fromList args
      , argsAnn = tokS.region
      , effects = effects <&> \row -> (row, row.ann)
      , result = result
      , resultAnn = tokE.region
      , ann = tokE.region
      }

tyEffectRow ::
  Parser (Surface.Type SourceRegion) ->
  Parser (Surface.EffectRowF Surface.Type SourceRegion, SourceRegion)
tyEffectRow pTy = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepBy effectAtom (single (Lexer.Punctuation Lexer.Comma))
  tailVar <- do
    if not $ null effects
      then do
        Megaparsec.choice
          [ Megaparsec.optional do
              _ <- single (Lexer.Punctuation Lexer.Colon)
              tokDD <- single (Lexer.Punctuation Lexer.DotDot)
              name <- anyTypeIdentifier pTy
              pure (name, SourceRegion{start = tokDD.region.start, end = name.ann.end})
          , Nothing <$ Megaparsec.optional (single (Lexer.Punctuation Lexer.DotDot))
          ]
      else Megaparsec.optional do
        tokDD <- single (Lexer.Punctuation Lexer.DotDot)
        name <- anyTypeIdentifier pTy
        pure (name, SourceRegion{start = tokDD.region.start, end = name.ann.end})
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    ( Surface.EffectRowF
        { effects = Vector.fromList effects
        , tailVar = fst <$> tailVar
        , ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
        }
    , SourceRegion{start = tokS.region.start, end = tokE.region.end}
    )
 where
  effectAtom :: Parser (Surface.EffectAtomF Surface.Type SourceRegion)
  effectAtom = do
    Megaparsec.choice
      [ eAtomNameType
      , eAtomScope
      , eAtomType
      ]
   where
    eAtomNameType :: Parser (Surface.EffectAtomF Surface.Type SourceRegion)
    eAtomNameType = do
      name <- simpleVarIdentifier
      _ <- single (Lexer.Punctuation Lexer.Colon)
      ty' <- pTy
      pure $
        Surface.EAtomNameTypeF
          name
          ty'
          SourceRegion
            { start = name.ann.start
            , end = ty'.ann.end
            }

    eAtomScope :: Parser (Surface.EffectAtomF Surface.Type SourceRegion)
    eAtomScope = do
      scope <- scopeIdentifier
      pure $
        Surface.EAtomScopeF
          scope
          SourceRegion
            { start = scope.ann.start
            , end = scope.ann.end
            }

    eAtomType :: Parser (Surface.EffectAtomF Surface.Type SourceRegion)
    eAtomType = do
      ty' <- pTy
      pure $ Surface.EAtomTypeF ty' SourceRegion{start = ty'.ann.start, end = ty'.ann.end}
