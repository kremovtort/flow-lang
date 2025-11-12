{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.Core.Module where

import Prelude hiding (readFile)

import "base" Data.Foldable (toList)
import "base" GHC.Generics (Generic)
import "base" GHC.Stack (HasCallStack)
import "containers" Data.Map.Strict (Map)
import "effectful" Effectful (Eff, (:>))
import "effectful" Effectful.FileSystem (FileSystem, doesFileExist)
import "effectful" Effectful.FileSystem.IO.ByteString (readFile)
import "filepath" System.FilePath (takeDirectory, (</>))
import "hashable" Data.Hashable (Hashable (..))
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "vector" Data.Vector (Vector)
import "unordered-containers" Data.HashMap.Strict (HashMap)

import Control.Monad (unless)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Void (Void)
import Effectful.Error.Static (Error, runErrorWith, throwError)
import Flow.AST.Ann (SourceSpan)
import Flow.AST.Surface (ModDefinitionBody)
import Flow.Core.Package (PackageId)
import Flow.Lexer qualified as Lexer
import Flow.Parser (pModDefinitionBody)
import GHC.Stack (CallStack)
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec qualified as Megaparsec
import Data.Sequence (Seq)

instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt salt = hashWithSalt salt . toList

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

parsePackageModules ::
  (HasCallStack, FileSystem :> es) =>
  PackageKind ->
  PackageId ->
  -- | Absolute path to the package src directory
  FilePath ->
  Eff es (Map ModuleId (ModDefinitionBody BasicAnn))
parsePackageModules packageKind packageId path = do
  let rootModulePath = case packageKind of
        PacKLib -> path </> "lib.fl"
        PacKExe -> path </> "main.fl"
  ast <-
    readModFile rootModulePath `handleError` rmfErrorHandler
  _
 where
  handleError action handler = runErrorWith handler action

  rmfErrorHandler :: CallStack -> ReadModFileError -> Eff es a
  rmfErrorHandler cs = \case
    RmfFileNotFound path' ->
      errorWith cs $
        "File not found: " <> path'
    RmfTextDecodingError path' err ->
      errorWith cs $
        "Text decoding error: " <> path' <> "\n" <> show err
    RmfLexerError path' err ->
      errorWith cs $
        "Lexer error: " <> path' <> "\n" <> Megaparsec.errorBundlePretty err
    RmfParserError path' err ->
      errorWith cs $
        "Parser error: " <> path' <> "\n" <> Megaparsec.errorBundlePretty err

  errorWith cs err =
    let ?callStack = cs in error err

data ReadModFileError
  = RmfFileNotFound FilePath
  | RmfTextDecodingError FilePath Text.Encoding.UnicodeException
  | RmfLexerError FilePath (ParseErrorBundle Text Void)
  | RmfParserError FilePath (ParseErrorBundle Lexer.TokenStream Void)
  deriving (Eq, Show)

readModFile ::
  ( HasCallStack
  , Error ReadModFileError :> es
  , FileSystem :> es
  ) =>
  FilePath ->
  Eff es (ModDefinitionBody BasicAnn)
readModFile path = do
  exists <- doesFileExist path
  unless exists do
    throwError $ RmfFileNotFound path
  bs <- readFile path
  text <- case Text.Encoding.decodeUtf8' bs of
    Left err -> throwError $ RmfTextDecodingError path err
    Right text -> pure text
  tokens <- case Megaparsec.runParser Lexer.tokensWithPos path text of
    Left err -> throwError $ RmfLexerError path err
    Right tokens -> pure tokens
  let tokenStream =
        Lexer.TokenStream
          { tokens = toList tokens
          , inputPos = Megaparsec.initialPos path
          , input = text
          }
  case Megaparsec.runParser pModDefinitionBody path tokenStream of
    Left err -> throwError $ RmfParserError path err
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
   in [ mod' rootDir (toList moduleName)
      , name rootDir (toList moduleName)
      ]

collectModuleDeclarations ::
  (HasCallStack) =>
  ModuleId ->
  ModDefinitionBody BasicAnn ->
  Eff es (Seq (ModuleId, ModuleVisibility))
collectModuleDeclarations modId body = do
  _
