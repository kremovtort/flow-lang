{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.Core.Module where

import Prelude hiding (readFile)

import "base" Control.Monad (unless, when)
import "base" Data.Either (partitionEithers)
import "base" Data.Foldable (foldlM, toList)
import "base" Data.Traversable (for)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import "base" GHC.Stack (CallStack, HasCallStack)
import "containers" Data.Map.Strict (Map)
import "containers" Data.Map.Strict qualified as Map
import "containers" Data.Sequence (Seq)
import "containers" Data.Sequence qualified as Seq
import "effectful" Effectful (Eff, (:>))
import "effectful" Effectful.Error.Static (Error, runError, runErrorWith, throwError)
import "effectful" Effectful.FileSystem (FileSystem, doesFileExist)
import "effectful" Effectful.FileSystem.IO.ByteString (readFile)
import "filepath" System.FilePath (takeDirectory, (</>))
import "hashable" Data.Hashable (Hashable (..))
import "megaparsec" Text.Megaparsec (ParseErrorBundle)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "text" Data.Text.Encoding qualified as Text.Encoding
import "text" Data.Text.Encoding.Error qualified as Text.Encoding
import "unordered-containers" Data.HashMap.Strict (HashMap)
import "unordered-containers" Data.HashSet (HashSet)
import "unordered-containers" Data.HashSet qualified as HashSet
import "vector" Data.Vector (Vector)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Ann (SourceSpan)
import Flow.AST.Surface (Mod (..), ModDefinitionBody)
import Flow.AST.Surface.Common (ModuleIdentifier (..))
import Flow.AST.Surface.Module (
  ModDefinitionBodyF (..),
  ModF (..),
  ModuleItemF (..),
  ModuleItemVariantF (..),
 )
import Flow.Core.Package (PackageId)
import Flow.Lexer qualified as Lexer
import Flow.Parser (pModDefinitionBody)

handleError ::
  (HasCallStack, Error e :> es) =>
  Eff (Error e : es) a ->
  (CallStack -> e -> Eff es a) ->
  Eff es a
handleError action handler = runErrorWith handler action

instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt salt = hashWithSalt salt . toList

instance (Hashable a) => Hashable (NonEmptyVector a) where
  hashWithSalt salt = hashWithSalt salt . NonEmptyVector.toList

data Visibility
  = -- | visible to all packages
    VisibilityAll
  | -- | visible to the package modules
    VisiblityPackage
  | -- | visible to the module and its submodules
    VisibilityModule ModuleId
  deriving (Eq, Ord, Show, Hashable, Generic)

data ModuleVisibility
  = ModVisAll
  | ModVisPackage
  | ModVisModule ModuleId
  deriving (Eq, Ord, Show, Hashable, Generic)

data ModuleId = ModuleId
  { package :: PackageId
  , name :: Vector Text
  }
  deriving (Eq, Ord, Show, Hashable, Generic)

newtype ModuleEnv = ModuleEnv
  { modules :: HashMap ModuleId ()
  }
  deriving (Eq, Ord, Show)

data BasicAnn = BasicAnn
  { path :: FilePath
  , span :: SourceSpan
  }
  deriving (Eq, Ord, Show)

data PackageKind
  = PacKLib
  | PacKExe
  deriving (Eq, Ord, Show)

type ReadAllModFilesErrors es =
  ( Error ModuleNotFoundError :> es
  , Error AmbiguousModuleLocationError :> es
  , Error DuplicateModuleDeclarationError :> es
  , ParseModuleErrors es
  )

readAllModFiles ::
  forall es.
  (HasCallStack, ReadAllModFilesErrors es, FileSystem :> es) =>
  PackageKind ->
  PackageId ->
  -- | Absolute path to the package src directory
  FilePath ->
  Eff es (Map ModuleId (Maybe BasicAnn, ModDefinitionBody BasicAnn))
readAllModFiles packageKind packageId packageSrcDir = do
  mods <- go Nothing Vector.empty
  pure $ Map.fromList (toList mods)
 where
  rootModulePath = case packageKind of
    PacKLib -> packageSrcDir </> "lib.fl"
    PacKExe -> packageSrcDir </> "main.fl"

  go :: Maybe BasicAnn -> Vector Text -> Eff es (Seq (ModuleId, (Maybe BasicAnn, ModDefinitionBody BasicAnn)))
  go mDeclAnn currentModPath = do
    let modPaths = case NonEmptyVector.fromVector currentModPath of
          Nothing -> [rootModulePath]
          Just currentModPath' -> moduleFilePathCandidates packageSrcDir currentModPath'
    (_, asts) <-
      partitionEithers <$> for modPaths \path' -> runError @FileNotFoundError do
        ast <- readModFile path'
        pure (path', ast)
    ast <- case asts of
      [] -> throwError $ ModuleNotFoundError mDeclAnn (ModuleId{package = packageId, name = currentModPath}) modPaths
      [(_, ast)] -> pure ast
      asts' ->
        throwError $
          AmbiguousModuleLocationError
            mDeclAnn
            ( ModuleId
                { package = packageId
                , name = currentModPath
                }
            )
            (fst <$> asts')
    decls <- collectModuleDeclarations (ModuleId{package = packageId, name = currentModPath}) ast
    mconcat <$> for (toList decls) \(declName, declAnn) -> do
      go (Just declAnn) (NonEmptyVector.toVector declName)

data ModuleNotFoundError = ModuleNotFoundError (Maybe BasicAnn) ModuleId [FilePath]
  deriving (Eq, Show)

data AmbiguousModuleLocationError = AmbiguousModuleLocationError (Maybe BasicAnn) ModuleId [FilePath]
  deriving (Eq, Show)

newtype FileNotFoundError = FileNotFoundError FilePath
  deriving (Eq, Show)

data TextDecodingError = TextDecodingError FilePath Text.Encoding.UnicodeException
  deriving (Eq, Show)

data LexerError = LexerError FilePath (ParseErrorBundle Text Void)
  deriving (Eq, Show)

data ParserError = ParserError FilePath (ParseErrorBundle Lexer.TokenStream Void)
  deriving (Eq, Show)

data DuplicateModuleDeclarationError = DuplicateModuleDeclarationError ModuleId BasicAnn
  deriving (Eq, Show)

type ParseModuleErrors es =
  ( Error TextDecodingError :> es
  , Error LexerError :> es
  , Error ParserError :> es
  )

type ReadModFileErrors es =
  ( Error FileNotFoundError :> es
  , ParseModuleErrors es
  )

readModFile ::
  ( HasCallStack
  , ReadModFileErrors es
  , FileSystem :> es
  ) =>
  FilePath ->
  Eff es (ModDefinitionBody BasicAnn)
readModFile path = do
  exists <- doesFileExist path
  unless exists do
    throwError $ FileNotFoundError path
  bs <- readFile path
  text <- case Text.Encoding.decodeUtf8' bs of
    Left err -> throwError $ TextDecodingError path err
    Right text -> pure text
  tokens <- case Megaparsec.runParser Lexer.tokensWithPos path text of
    Left err -> throwError $ LexerError path err
    Right tokens -> pure tokens
  let tokenStream =
        Lexer.TokenStream
          { tokens = toList tokens
          , inputPos = Megaparsec.initialPos path
          , input = text
          }
  case Megaparsec.runParser pModDefinitionBody path tokenStream of
    Left err -> throwError $ ParserError path err
    Right ast -> pure $ BasicAnn path <$> ast

moduleFilePathCandidates ::
  (HasCallStack) =>
  -- | Absolute path to the package src directory
  FilePath ->
  -- | Module name
  NonEmptyVector Text ->
  [FilePath]
moduleFilePathCandidates rootPath moduleName =
  let rootDir = takeDirectory rootPath

      mod' _ [] = error "unreachable"
      mod' path [lastChunk] = path </> Text.unpack lastChunk </> "mod.fl"
      mod' path (chunk : chunks) = mod' (path </> Text.unpack chunk) chunks

      name _ [] = error "unreachable"
      name path [lastChunk] = path </> (Text.unpack lastChunk <> ".fl")
      name path (chunk : chunks) = name (path </> Text.unpack chunk) chunks
  in  ---
      [ mod' rootDir (toList moduleName)
      , name rootDir (toList moduleName)
      ]

collectModuleDeclarations ::
  forall es.
  (HasCallStack, Error DuplicateModuleDeclarationError :> es) =>
  ModuleId ->
  ModDefinitionBody BasicAnn ->
  Eff es (Seq (NonEmptyVector Text, BasicAnn))
collectModuleDeclarations modId body = do
  (decls, _) <- collectFromBody modId.name HashSet.empty body
  pure decls
 where
  collectFromBody ::
    (HasCallStack) =>
    Vector Text ->
    HashSet (NonEmptyVector Text) ->
    ModDefinitionBody BasicAnn ->
    Eff es (Seq (NonEmptyVector Text, BasicAnn), HashSet (NonEmptyVector Text))
  collectFromBody parent seen body' =
    foldlM
      (collectFromItem parent)
      (Seq.empty, seen)
      (toList moduleItems)
   where
    ModDefinitionBodyF{items = moduleItems} = body'

  collectFromItem parent (acc, seen) ModuleItemF{item = variant} =
    case variant of
      ModItemModF nested ->
        collectFromMod parent nested (acc, seen)
      _ -> pure (acc, seen)

  collectFromMod parent Mod{mod = mod', ann = modAnn} (acc, seen) =
    case mod' of
      ModDeclarationF ident -> do
        let childId = NonEmptyVector.snocV parent ident.name
        when (HashSet.member childId seen) do
          throwError $
            DuplicateModuleDeclarationError
              ( ModuleId
                  { package = modId.package
                  , name = NonEmptyVector.toVector childId
                  }
              )
              modAnn
        pure (acc Seq.|> (childId, modAnn), HashSet.insert childId seen)
      ModDefinitionF ident nestedBody -> do
        let childId = NonEmptyVector.snocV parent ident.name
        when (HashSet.member childId seen) do
          throwError $
            DuplicateModuleDeclarationError
              ( ModuleId
                  { package = modId.package
                  , name = NonEmptyVector.toVector childId
                  }
              )
              modAnn
        let seen' = HashSet.insert childId seen
        (nestedDecls, seen'') <- collectFromBody (NonEmptyVector.toVector childId) seen' nestedBody
        pure (acc Seq.>< nestedDecls, seen'')
