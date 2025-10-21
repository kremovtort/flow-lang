{-# LANGUAGE DuplicateRecordFields #-}

module Flow.Parser where

import Flow.AST.Surface qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Expr qualified as PExpr
import Flow.Parser.Module qualified as PMod
import Flow.Parser.Pattern qualified as PPat
import Flow.Parser.Syntax qualified as PSyn
import Flow.Parser.Type qualified as PType
import "base" Data.Foldable (toList)
import "megaparsec" Text.Megaparsec (parse)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "text" Data.Text (Text)


