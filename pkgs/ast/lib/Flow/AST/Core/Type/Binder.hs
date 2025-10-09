module Flow.AST.Core.Type.Binder where

import "base" Data.List qualified as List
import "base" Numeric.Natural (Natural)
import "containers" Data.Set qualified as Set
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Core.Type.Common (Name)
import Flow.AST.Core.Type.Universe (Sort)

{- | Universe level can be a concrete natural, a named level variable,
a max of non-empty levels, or a successor of a level.
-}
data UniverseLevel
  = LevelConst Natural
  | LevelVar Name
  | LevelMax (NonEmptyVector UniverseLevel)
  | LevelSucc UniverseLevel
  deriving (Eq, Ord, Show)

-- | Explicit universe annotation used in Core AST binders and sorts.
data UniverseAnn = UniverseAnn
  { sort :: Sort
  , level :: UniverseLevel
  }
  deriving (Eq, Ord, Show)

-- | Type-level binder with explicit universe sort and level.
data TyBinder = TyBinder
  { binderName :: Name
  , binderUniverse :: UniverseAnn
  }
  deriving (Eq, Ord, Show)

-- | Normalize a universe level into a canonical form.
normalizeUniverseLevel :: UniverseLevel -> UniverseLevel
normalizeUniverseLevel level0 = case level0 of
  LevelConst n -> LevelConst n
  LevelVar v -> LevelVar v
  LevelSucc l ->
    case normalizeUniverseLevel l of
      LevelConst n -> LevelConst (n + 1)
      l' -> LevelSucc l'
  LevelMax ne ->
    let
      normVec = Vector.map normalizeUniverseLevel (NonEmptyVector.toVector ne)
      flattenOne acc e = case e of
        LevelMax ne' -> acc Vector.++ NonEmptyVector.toVector ne'
        _ -> acc `Vector.snoc` e
      flattened = Vector.foldl' flattenOne Vector.empty normVec
      dedupOrdered vec =
        let go seen xs = case xs Vector.!? 0 of
              Nothing -> Vector.empty
              Just x ->
                if Set.member x seen
                  then go seen (Vector.tail xs)
                  else Vector.cons x (go (Set.insert x seen) (Vector.tail xs))
         in go Set.empty vec
      deduped = dedupOrdered flattened
      sorted = Vector.fromList (List.sort (Vector.toList deduped))
     in
      case NonEmptyVector.fromVector sorted of
        Nothing -> case Vector.toList deduped of
          [] -> LevelMax ne
          [x] -> x
          xs -> case NonEmptyVector.fromVector (Vector.fromList xs) of
            Just ne' -> LevelMax ne'
            Nothing -> LevelMax ne
        Just ne' -> if NonEmptyVector.length ne' == 1 then NonEmptyVector.head ne' else LevelMax ne'
