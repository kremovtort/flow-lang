module Flow.AST.Surface.Literal where

import "scientific" Data.Scientific (Scientific)
import "text" Data.Text (Text)
import "bytestring" Data.ByteString (ByteString)
import "base" Data.Word (Word8)

data Literal ann
  = LitUnit ann
  | LitBool Bool ann
  | LitInteger Integer ann
  | LitFloat Scientific ann
  | LitByte Word8 ann
  | LitByteString ByteString ann
  | LitChar Char ann
  | LitString Text ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
