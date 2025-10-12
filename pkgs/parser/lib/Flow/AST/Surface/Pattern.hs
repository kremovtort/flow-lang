module Flow.AST.Surface.Pattern where

import "base" Prelude hiding (Enum)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
-- imports only for instances; keep empty to avoid unused warnings
import "scientific" Data.Scientific ()
import "text" Data.Text ()
import "bytestring" Data.ByteString ()
import "base" Data.Word ()
import "base" Data.Char ()

import Flow.AST.Common (SimpleVarIdentifier, AnyTypeIdentifier)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Fields (Fields)

data Pattern
  = PatternWildcard -- _
  | PatternLiteral Literal -- 0 | true | "str"
  | PatternVar SimpleVarIdentifier -- x
  | PatternTuple (NonEmptyVector Pattern) -- (a, b, c)
  | PatternCons (ConstructorApp Pattern) -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | PatternOr (NonEmptyVector Pattern) -- a | b | c
  deriving (Eq, Ord, Show)

data ConstructorApp t = ConstructorApp
  { name :: AnyTypeIdentifier
  , fields :: Fields t
  }
  deriving (Eq, Ord, Show)
