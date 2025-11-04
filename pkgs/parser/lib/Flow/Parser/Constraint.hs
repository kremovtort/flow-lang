{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Constraint where

import "base" Control.Monad (unless, when)
import "base" Data.Functor (void)
import "base" Data.Maybe (fromJust)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint (ScopeBinderWoConstraintsF (..))
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  moduleIdentifier,
  scopeIdentifier,
  simpleTypeIdentifier,
  simpleVarIdentifier,
  single,
 )

pBindersApp ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.BindersAppF ty Lexer.SourceRegion)
pBindersApp pTy = Megaparsec.label "binders app" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  elems <- Megaparsec.sepEndBy1 binderElem (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  (scopeElems, typeElems) <- collect elems [] []
  let scopesVector = Vector.fromList (ScopeBinderWoConstraintsF <$> scopeElems)
      typesVector = Vector.fromList (Surface.BinderAppF <$> typeElems)
  pure
    Surface.BindersF
      { scopes = scopesVector
      , types = typesVector
      , ann = Lexer.SourceRegion tokS.region.start tokE.region.end
      }
 where
  binderElem =
    Megaparsec.choice
      [ Left <$> Megaparsec.try scopeIdentifier
      , Right <$> pTy
      ]

  collect [] scopesAcc typesAcc =
    pure (reverse scopesAcc, reverse typesAcc)
  collect (Left scope : rest) scopesAcc typesAcc
    | not (null typesAcc) =
        fail "scope argument cannot follow type argument in application binders"
    | otherwise =
        collect rest (scope : scopesAcc) typesAcc
  collect (Right ty : rest) scopesAcc typesAcc =
    collect rest scopesAcc (ty : typesAcc)

anyTypeIdentifier ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.AnyTypeIdentifier ty Lexer.SourceRegion)
anyTypeIdentifier pTy = do
  qualifier <- Megaparsec.many (Megaparsec.try (moduleIdentifier <* moduleSeparator))
  typeQualifier <- Megaparsec.optional $ Megaparsec.try do
    typeName <- simpleTypeIdentifier
    typeParams <- pBindersApp pTy
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    pure
      Surface.TypeQualifierF
        { typeName
        , typeParams
        }
  identifier <- simpleTypeIdentifier
  pure $
    Surface.AnyTypeIdentifier
      { qualifier = NonEmptyVector.fromList qualifier
      , typeQualifier
      , identifier
      , ann =
          Lexer.SourceRegion
            { start = case qualifier of
                [] -> identifier.ann.start
                q : _ -> q.ann.start
            , end = identifier.ann.end
            }
      }
 where
  moduleSeparator = single (Lexer.Punctuation Lexer.ColonColon)

anyVarIdentifier ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.AnyVarIdentifier ty Lexer.SourceRegion)
anyVarIdentifier pTy = do
  qualifier <- Megaparsec.many (Megaparsec.try (moduleIdentifier <* moduleSeparator))
  typeQualifier <- Megaparsec.optional do
    typeName <- simpleTypeIdentifier
    typeParams <- pBindersApp pTy
    pure
      Surface.TypeQualifierF
        { typeName
        , typeParams
        }
  identifier <- simpleVarIdentifier
  pure $
    Surface.AnyVarIdentifier
      { qualifier = NonEmptyVector.fromList qualifier
      , typeQualifier = typeQualifier
      , identifier = identifier
      , ann =
          Lexer.SourceRegion
            { start = case qualifier of
                [] -> identifier.ann.start
                q : _ -> q.ann.start
            , end = identifier.ann.end
            }
      }
 where
  moduleSeparator = single (Lexer.Punctuation Lexer.ColonColon)

pKindTreeRoot ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.KindTreeRootF ty Lexer.SourceRegion, Lexer.SourceRegion)
pKindTreeRoot pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  trees <- Megaparsec.sepEndBy1 pKindTree (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    ( fromJust $ NonEmptyVector.fromList trees
    , Lexer.SourceRegion
        { start = tokS.region.start
        , end = tokE.region.end
        }
    )
 where
  pKindTree =
    Megaparsec.choice
      [ Megaparsec.try pKindParams
      , pKindHole
      ]

  pKindHole = do
    tokS <- single (Lexer.Punctuation Lexer.Underscore)
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.KTHoleF
        Surface.KindHoleF
          { holeAnn = tokS.region
          , typeType = typeType
          , ann =
              Lexer.SourceRegion
                { start = tokS.region.start
                , end = case typeType of
                    Just ty -> ty.ann.end
                    Nothing -> tokS.region.end
                }
          }
  pKindParams = do
    tokS <- single (Lexer.Punctuation Lexer.Underscore)
    _ <- single (Lexer.Punctuation Lexer.LessThan)
    params <- Megaparsec.sepEndBy1 pKindTree (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.KTParamsF
        Surface.KindParamsF
          { holeAnn = tokS.region
          , params = fromJust $ NonEmptyVector.fromList params
          , typeType
          , ann =
              Lexer.SourceRegion
                { start = tokS.region.start
                , end = case typeType of
                    Just ty -> ty.ann.end
                    Nothing -> tokE.region.end
                }
          }

pBindersWoConstraints ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.BindersWoConstraintsF ty Lexer.SourceRegion)
pBindersWoConstraints pTy = Megaparsec.label "binders with constraints" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  scopeBinders <- Megaparsec.sepBy scopeIdentifier (single (Lexer.Punctuation Lexer.Comma))
  unless (null scopeBinders) do
    void $ single (Lexer.Punctuation Lexer.Comma)
  typeBinders <- Megaparsec.sepBy pBinderWoConstraints (single (Lexer.Punctuation Lexer.Comma))
  when (null scopeBinders && null typeBinders) do
    fail "Expected at least one scope or type binder"
  _ <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Comma)
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    Surface.BindersF
      { scopes = Vector.fromList (fmap Surface.ScopeBinderWoConstraintsF scopeBinders)
      , types = Vector.fromList typeBinders
      , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }
 where
  pBinderWoConstraints = do
    name <- simpleTypeIdentifier
    kindShort <- Megaparsec.optional (pKindTreeRoot pTy)
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.BinderWoConstraintF
        { name = name
        , kindShort = fst <$> kindShort
        , typeType = typeType
        , ann =
            Lexer.SourceRegion
              { start = name.ann.start
              , end = case typeType of
                  Just ty -> ty.ann.end
                  Nothing -> case kindShort of
                    Just (_, ann) -> ann.end
                    Nothing -> name.ann.end
              }
        }

pBindersWConstraints ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.BindersWConstraintsF ty Lexer.SourceRegion)
pBindersWConstraints pTy = Megaparsec.label "binders with constraints" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  scopeBinders <- Megaparsec.sepBy pScopeBinderWConstraints (single (Lexer.Punctuation Lexer.Comma))
  unless (null scopeBinders) do
    void $ single (Lexer.Punctuation Lexer.Comma)
  typeBinders <- Megaparsec.sepBy pBinderWConstraints (single (Lexer.Punctuation Lexer.Comma))
  when (null scopeBinders && null typeBinders) do
    fail "Expected at least one scope or type binder"
  _ <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Comma)
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure $
    Surface.BindersF
      { scopes = Vector.fromList scopeBinders
      , types = Vector.fromList typeBinders
      , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }
 where
  pScopeBinderWConstraints = do
    name <- scopeIdentifier
    constraint <- Megaparsec.optional pBinderConstraints
    pure $
      Surface.ScopeBinderWConstraintsF
        { name
        , constraint
        , ann =
            Lexer.SourceRegion
              { start = name.ann.start
              , end = case constraint of
                  Just cnst -> cnst.ann.end
                  Nothing -> name.ann.end
              }
        }

  pBinderWConstraints = do
    name <- simpleTypeIdentifier
    kindShort <- Megaparsec.optional (pKindTreeRoot pTy)
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    constraint <- Megaparsec.optional pBinderConstraints
    pure
      Surface.BinderWConstraintsF
        { name = name
        , kindShort = fst <$> kindShort
        , typeType = typeType
        , constraint = constraint
        , ann =
            Lexer.SourceRegion
              { start = name.ann.start
              , end = case constraint of
                  Just cnst -> cnst.ann.end
                  Nothing -> case typeType of
                    Just ty -> ty.ann.end
                    Nothing -> case kindShort of
                      Just (_, ann) -> ann.end
                      Nothing -> name.ann.end
              }
        }

  pBinderConstraints = do
    tokS <- single (Lexer.Punctuation Lexer.ColonLessThan)
    constraints <- Megaparsec.sepBy1 (anyTypeIdentifier pTy) (single (Lexer.Punctuation Lexer.Plus))
    pure
      Surface.BinderConstraintsF
        { constraints = fromJust $ NonEmptyVector.fromList constraints
        , ann =
            Lexer.SourceRegion
              { start = tokS.region.start
              , end = (last constraints).ann.end
              }
        }

pWhereBlockHead ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.WhereBlockF ty Lexer.SourceRegion)
pWhereBlockHead pTy = do
  tokS <- single (Lexer.Keyword Lexer.Where)
  clauses <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 (pWhereClause pTy) (single (Lexer.Punctuation Lexer.Comma))
  pure $
    Surface.WhereBlockF
      { clauses = fmap fst clauses
      , ann =
          Lexer.SourceRegion
            { start = tokS.region.start
            , end = (snd $ NonEmptyVector.last clauses).end
            }
      }

pWhereBlockNested ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.WhereBlockF ty Lexer.SourceRegion)
pWhereBlockNested pTy = do
  tokS <- single (Lexer.Keyword Lexer.Where)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  clauses <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 (pWhereClause pTy) (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure $
    Surface.WhereBlockF
      { clauses = fmap fst clauses
      , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }

pWhereClause ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.WhereClauseF ty Lexer.SourceRegion, Lexer.SourceRegion)
pWhereClause pTy =
  Megaparsec.choice
    [ pWhereConstraint
    , pWhereAlias
    ]
 where
  pWhereConstraint = do
    ty <- pTy
    pure (Surface.WhereConstraintF ty, ty.ann)
  pWhereAlias = do
    typeDefinition <- pTypeDefinition pTy
    pure (Surface.WhereAliasF typeDefinition, typeDefinition.ann)

pTypeDefinition ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.TypeDefinitionF ty Lexer.SourceRegion)
pTypeDefinition pTy = do
  tokS <- single (Lexer.Keyword Lexer.Type)
  name <- simpleTypeIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  _ <- single (Lexer.Punctuation Lexer.Assign)
  type_ <- pTy
  pure $
    Surface.TypeDefinitionF
      { name
      , typeParams
      , type_
      , ann =
          Lexer.SourceRegion
            { start = tokS.region.start
            , end = type_.ann.end
            }
      }
