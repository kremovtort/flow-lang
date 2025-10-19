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
import Flow.AST.Surface.Type (TypeF (TyEffectRowF))
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, anyTypeIdentifier, scopeIdentifier, simpleVarIdentifier, single, token)

-- Parse an atomic type (no application postfixed).
pTypeAtom :: Parser (Surface.Type Lexer.SourceRegion)
pTypeAtom =
  Megaparsec.choice
    [ (\(b, ann) -> Surface.Type{ty = Surface.TyBuiltinF b ann, ann = ann}) <$> tyBuiltin
    , (\i -> Surface.Type{ty = Surface.TyIdentifierF i, ann = i.ann}) <$> tyIdentifier
    , (\(args, ann) -> Surface.Type{ty = Surface.TyTupleF args ann, ann = ann}) <$> tyTuple pType
    , (\r -> Surface.Type{ty = Surface.TyRefF r, ann = r.ann}) <$> tyRef
    , (\fn -> Surface.Type{ty = Surface.TyFnF fn, ann = fn.ann}) <$> tyFn pType
    , (\(row, ann) -> Surface.Type{ty = Surface.TyEffectRowF row, ann}) <$> tyEffectRow pType
    -- TODO: forall
    ]

-- Top-level type parser: parse an atom, then optionally parse application(s)
-- as postfix forms. This avoids left recursion by ensuring the head is
-- consumed before parsing any applications.
pType :: Parser (Surface.Type Lexer.SourceRegion)
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
                        , headAnn = head'.ann
                        , args = NonEmptyVector.singleton inner
                        , argsAnn = inner.ann
                        , ann = inner.ann
                        }
                , ann = inner.ann
                }
        , pure head'
        ]
    _ -> do
      Megaparsec.choice
        [ do
            tokS <- single (Lexer.Punctuation Lexer.LessThan)
            args <- Megaparsec.sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
            tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
            pure $
              Surface.Type
                { ty =
                    Surface.TyAppF
                      Surface.AppF
                        { head = head'
                        , headAnn = head'.ann
                        , args = fromJust $ NonEmptyVector.fromList args
                        , argsAnn = Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
                        , ann = Lexer.SourceRegion{start = head'.ann.start, end = tokE.payload.end}
                        }
                , ann = Lexer.SourceRegion{start = head'.ann.start, end = tokE.payload.end}
                }
        , pure head'
        ]

tyBuiltin :: Parser (Surface.Builtin, Lexer.SourceRegion)
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
  pure (tok.token, tok.payload)

tyIdentifier :: Parser (Surface.AnyTypeIdentifier Lexer.SourceRegion)
tyIdentifier = anyTypeIdentifier

tyTuple ::
  Parser (Surface.Type Lexer.SourceRegion) ->
  Parser (NonEmptyVector (Surface.Type Lexer.SourceRegion), Lexer.SourceRegion)
tyTuple ty = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy1 ty (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( fromJust $ NonEmptyVector.fromList args
    , Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
    )

tyRef :: Parser (Surface.RefF Lexer.SourceRegion)
tyRef = do
  tokS <- single (Lexer.Punctuation Lexer.Ampersand)
  scope <- Megaparsec.optional $ do
    tok <-
      token (Set.singleton $ Megaparsec.Label "scope identifier") \case
        Lexer.RefScope i -> Just i
        _ -> Nothing
    pure $ Surface.ScopeIdentifier{name = tok.token, ann = tok.payload}
  mut <- Megaparsec.optional $ do
    tok <- single (Lexer.Keyword Lexer.Mut)
    pure Lexer.SourceRegion{start = tok.payload.start, end = tok.payload.end}
  pure $
    Surface.RefF
      { scope = scope
      , mutability = mut
      , ann =
          Lexer.SourceRegion
            { start = tokS.payload.start
            , end = case mut of
                Just mut' -> mut'.end
                Nothing -> case scope of
                  Just scope' -> scope'.ann.end
                  Nothing -> tokS.payload.end
            }
      }

tyFn ::
  Parser (Surface.Type Lexer.SourceRegion) ->
  Parser (Surface.FnF Surface.Type Lexer.SourceRegion)
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
      , argsAnn = tokS.payload
      , effects = effects <&> \row -> (row, row.ann)
      , result = result
      , resultAnn = tokE.payload
      , ann = tokE.payload
      }

tyEffectRow ::
  Parser (Surface.Type Lexer.SourceRegion) ->
  Parser (Surface.EffectRowF Surface.Type Lexer.SourceRegion, Lexer.SourceRegion)
tyEffectRow ty = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepBy effectAtom (single (Lexer.Punctuation Lexer.Comma))
  tailVar <- do
    if not $ null effects
      then do
        Megaparsec.choice
          [ Megaparsec.optional do
            _ <- single (Lexer.Punctuation Lexer.Colon)
            tokDD <- single (Lexer.Punctuation Lexer.DotDot)
            name <- anyTypeIdentifier
            pure (name, Lexer.SourceRegion{start = tokDD.payload.start, end = name.ann.end})
          , Nothing <$ Megaparsec.optional (single (Lexer.Punctuation Lexer.DotDot))
          ]
      else Megaparsec.optional do
        tokDD <- single (Lexer.Punctuation Lexer.DotDot)
        name <- anyTypeIdentifier
        pure (name, Lexer.SourceRegion{start = tokDD.payload.start, end = name.ann.end})
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    ( Surface.EffectRowF
        { effects = Vector.fromList effects
        , tailVar = fst <$> tailVar
        , ann = Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
        }
    , Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
    )
 where
  effectAtom :: Parser (Surface.EffectAtomF Surface.Type Lexer.SourceRegion)
  effectAtom = do
    Megaparsec.choice
      [ eAtomNameType
      , eAtomScope
      , eAtomType
      ]
   where
    eAtomNameType :: Parser (Surface.EffectAtomF Surface.Type Lexer.SourceRegion)
    eAtomNameType = do
      name <- simpleVarIdentifier
      _ <- single (Lexer.Punctuation Lexer.Colon)
      ty' <- ty
      pure $
        Surface.EAtomNameTypeF
          name
          ty'
          Lexer.SourceRegion
            { start = name.ann.start
            , end = ty'.ann.end
            }

    eAtomScope :: Parser (Surface.EffectAtomF Surface.Type Lexer.SourceRegion)
    eAtomScope = do
      scope <- scopeIdentifier
      pure $
        Surface.EAtomScopeF
          scope
          Lexer.SourceRegion
            { start = scope.ann.start
            , end = scope.ann.end
            }

    eAtomType :: Parser (Surface.EffectAtomF Surface.Type Lexer.SourceRegion)
    eAtomType = do
      ty' <- ty
      pure $ Surface.EAtomTypeF ty' Lexer.SourceRegion{start = ty'.ann.start, end = ty'.ann.end}
