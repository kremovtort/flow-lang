module Flow.Parser.Lexer (
  Token (..),
  Keyword (..),
  Punctuation (..),
  TokenWith (..),
  TokenWithSourcePosition,
  Lexer,
  tokens,
  token,
  tokensWithSourcePos,
  tokenWithSourcePos,
) where

import "base" Control.Applicative (many, (<|>))
import "base" Data.Char (chr, isAlphaNum, isDigit, isHexDigit, isOctDigit)
import "base" Data.List qualified as List
import "base" Data.Ord qualified as Ord
import "base" Data.Void (Void)
import "base" Data.Word (Word8)
import "base" Text.Read (readEither)
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString qualified as ByteString
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec (Parsec)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "megaparsec" Text.Megaparsec.Char qualified as Megaparsec.Char
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Megaparsec.Char.Lexer
import "scientific" Data.Scientific (Scientific)
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "vector" Data.Vector (Vector)
import "vector" Data.Vector qualified as Vector

type Lexer = Parsec Void Text

data TokenWith a = TokenWith {token :: Token, payload :: a}
  deriving (Eq, Ord, Show, Functor)

type TokenWithSourcePosition = TokenWith (Megaparsec.SourcePos, Megaparsec.SourcePos)

data Token
  = Keyword Keyword
  | Punctuation Punctuation
  | Identifier Text
  | Optic Text
  | RefScope Text
  | BoolLiteral Bool
  | IntegerLiteral Integer
  | FloatLiteral Scientific
  | ByteLiteral Word8
  | ByteStringLiteral ByteString
  | CharLiteral Char
  | StringLiteral Text
  deriving (Eq, Ord, Show)

data Keyword
  = Abort
  | Do
  | Effect
  | Else
  | Enum
  | Fn
  | For
  | Handle
  | If
  | Impl
  | In
  | Let
  | Loop
  | Match
  | Mod
  | Mut
  | Op
  | Pub
  | Recover
  | Resume
  | Return
  | Returning
  | Struct
  | Trait
  | Type
  | Use
  | Where
  | While
  deriving (Eq, Ord, Show, Bounded, Enum)

data Punctuation
  = Assign
  | LessThan
  | LessThanOrEqual
  | Equal
  | NotEqual
  | GreaterThanOrEqual
  | GreaterThan
  | And
  | Or
  | Not
  | Tilde
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Ampersand
  | Pipe
  | ShiftLeft
  | ShiftRight
  | PlusAssign
  | MinusAssign
  | StarAssign
  | SlashAssign
  | PercentAssign
  | CaretAssign
  | AmpersandAssign
  | PipeAssign
  | ShiftLeftAssign
  | ShiftRightAssign
  | Inherit
  | At
  | Dot
  | DotDot
  | DotDotDot
  | DotDotEqual
  | Comma
  | Semicolon
  | Colon
  | ColonColon
  | Arrow
  | LeftArrow
  | FatArrow
  | Hash
  | Dollar
  | Question
  | Underscore
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | LeftParen
  | RightParen
  deriving (Eq, Ord, Show, Bounded, Enum)

tokens :: Lexer (Vector Token)
tokens = fmap Vector.fromList $ spaceConsumer *> many token <* Megaparsec.eof

tokensWithSourcePos :: Lexer (Vector TokenWithSourcePosition)
tokensWithSourcePos = fmap Vector.fromList $ spaceConsumer *> many tokenWithSourcePos

token :: Lexer Token
token =
  Megaparsec.choice
    [ Megaparsec.try keyword
    , Megaparsec.try boolLiteral
    , Megaparsec.try integerLiteral
    , floatLiteral
    , byteLiteral
    , byteStringLiteral
    , Megaparsec.try charLiteral
    , stringLiteral
    , Megaparsec.try refScope
    , Megaparsec.try optic
    , punctuation
    , identifier
    ]

tokenWithSourcePos :: Lexer TokenWithSourcePosition
tokenWithSourcePos = do
  startPos <- Megaparsec.getSourcePos
  token' <- token
  endPos <- Megaparsec.getSourcePos
  pure $ TokenWith{token = token', payload = (startPos, endPos)}

keyword :: Lexer Token
keyword = Keyword <$> Megaparsec.choice keywords
 where
  keywords = map mkKeyword $ List.sortOn (Ord.Down . keywordText) (enumFrom Abort)

punctuation :: Lexer Token
punctuation = Punctuation <$> Megaparsec.choice punctuations
 where
  punctuations = map mkPunctuation $ List.sortOn (Ord.Down . punctuationText) (enumFrom Assign)

identifier :: Lexer Token
identifier =
  Identifier <$> lexeme do
    fstChar <- Megaparsec.Char.letterChar <|> Megaparsec.Char.char '_'
    restChars <- Megaparsec.takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
    pure $ Text.cons fstChar restChars

refScope :: Lexer Token
refScope =
  RefScope <$> lexeme do
    _ <- Megaparsec.Char.char '\''
    fstChar <- Megaparsec.Char.letterChar <|> Megaparsec.Char.char '_'
    restChars <- Megaparsec.takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
    pure $ Text.cons fstChar restChars

optic :: Lexer Token
optic =
  Optic <$> lexeme do
    _ <- Megaparsec.Char.string "#"
    fstChar <- Megaparsec.Char.letterChar <|> Megaparsec.Char.char '_'
    restChars <- Megaparsec.takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
    pure $ Text.cons fstChar restChars

boolLiteral :: Lexer Token
boolLiteral =
  BoolLiteral <$> ((trueLiteral <|> falseLiteral) <* Megaparsec.notFollowedBy alphaNumUnderscoreChar)
 where
  trueLiteral = True <$ lexeme (Megaparsec.Char.string "true")
  falseLiteral = False <$ lexeme (Megaparsec.Char.string "false")

integerLiteral :: Lexer Token
integerLiteral =
  IntegerLiteral <$> lexeme do
    val <-
      Megaparsec.choice
        [ binLiteral
        , octLiteral
        , hexLiteral
        , decLiteral
        ]
    Megaparsec.notFollowedBy (Megaparsec.try alphaNumUnderscoreChar <|> Megaparsec.Char.char '.')
    pure val

floatLiteral :: Lexer Token
floatLiteral =
  FloatLiteral <$> lexeme do
    val <- Megaparsec.Char.Lexer.scientific
    Megaparsec.notFollowedBy alphaNumUnderscoreChar
    pure val

byteLiteral :: Lexer Token
byteLiteral =
  ByteLiteral <$> lexeme do
    Megaparsec.between (Megaparsec.Char.string "b'") (Megaparsec.Char.char '\'') do
      Megaparsec.choice [byteChar, byteEscape]
 where
  byteChar = fromIntegral . fromEnum <$> Megaparsec.oneOf asciiChars
  asciiChars =
    Set.difference
      (Set.fromList (map chr [0x00 .. 0x7F]))
      (Set.fromList ['\'', '\\', '\n', '\r', '\t'])

byteStringLiteral :: Lexer Token
byteStringLiteral =
  ByteStringLiteral . ByteString.pack <$> lexeme do
    Megaparsec.between (Megaparsec.Char.string "b\"") (Megaparsec.Char.char '"') do
      many $ Megaparsec.choice [byteChar, byteEscape, byteStringContinue]
 where
  byteChar = fromIntegral . fromEnum <$> Megaparsec.oneOf asciiChars
  asciiChars =
    Set.difference
      (Set.fromList (map chr [0x00 .. 0x7F]))
      (Set.fromList ['\"', '\\', '\r'])
  byteStringContinue = (fromIntegral . fromEnum) '\n' <$ Megaparsec.Char.string "\\\n"

byteEscape :: Lexer Word8
byteEscape =
  Megaparsec.choice
    [ charToWord8 '\n' <$ Megaparsec.Char.string "\\n"
    , charToWord8 '\r' <$ Megaparsec.Char.string "\\r"
    , charToWord8 '\t' <$ Megaparsec.Char.string "\\t"
    , charToWord8 '\\' <$ Megaparsec.Char.string "\\\\"
    , charToWord8 '\0' <$ Megaparsec.Char.string "\\0"
    , do
        _ <- Megaparsec.Char.string "\\x"
        digit1 <- Megaparsec.Char.hexDigitChar
        digit2 <- Megaparsec.Char.hexDigitChar
        case readEither @Word8 ("0x" <> [digit1, digit2]) of
          Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
          Right n -> pure n
    ]
 where
  charToWord8 :: Char -> Word8
  charToWord8 = fromIntegral . fromEnum

charLiteral :: Lexer Token
charLiteral =
  CharLiteral <$> lexeme do
    Megaparsec.between (Megaparsec.Char.char '\'') (Megaparsec.Char.char '\'') do
      Megaparsec.choice [char, quoteEscape, asciiEscape, unicodeEscape]
 where
  char = Megaparsec.noneOf ['\'', '\\', '\n', '\r', '\t']

stringLiteral :: Lexer Token
stringLiteral = lexeme do
  StringLiteral . Text.pack <$> Megaparsec.between (Megaparsec.Char.char '"') (Megaparsec.Char.char '"') do
    many (Megaparsec.choice [char, quoteEscape, asciiEscape, unicodeEscape, stringContinue])
 where
  char = Megaparsec.noneOf ['\"', '\\', '\r']
  stringContinue = '\n' <$ Megaparsec.Char.string "\\\n"

quoteEscape :: Lexer Char
quoteEscape =
  Megaparsec.choice
    [ '\'' <$ Megaparsec.Char.string "\\'"
    , '\"' <$ Megaparsec.Char.string "\\\""
    ]

asciiEscape :: Lexer Char
asciiEscape =
  Megaparsec.choice
    [ '\n' <$ Megaparsec.Char.string "\\n"
    , '\r' <$ Megaparsec.Char.string "\\r"
    , '\t' <$ Megaparsec.Char.string "\\t"
    , '\\' <$ Megaparsec.Char.string "\\\\"
    , '\0' <$ Megaparsec.Char.string "\\0"
    , do
        _ <- Megaparsec.Char.string "\\x"
        octDigit <- Megaparsec.Char.octDigitChar
        hexDigit <- Megaparsec.Char.hexDigitChar
        case readEither ("0x" <> [octDigit, hexDigit]) of
          Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
          Right n -> pure $ chr n
    ]

unicodeEscape :: Lexer Char
unicodeEscape = Megaparsec.between (Megaparsec.Char.string "\\u{") (Megaparsec.Char.char '}') do
  digits <- Megaparsec.takeWhileP Nothing (\c -> isHexDigit c || c == '_')
  case readEither ("0x" <> List.filter (/= '_') (Text.unpack digits)) of
    Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
    Right n
      | n >= 0 && n <= 0x10FFFF -> pure $ chr n
      | otherwise -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail "Invalid Unicode code point"

binLiteral :: Lexer Integer
binLiteral = lexeme do
  _ <- Megaparsec.Char.string "0b"
  digits <- Megaparsec.takeWhileP Nothing (\c -> c == '0' || c == '1' || c == '_')
  case readEither ("0b" <> List.filter (/= '_') (Text.unpack digits)) of
    Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
    Right n -> pure n

octLiteral :: Lexer Integer
octLiteral = lexeme do
  _ <- Megaparsec.Char.string "0o"
  digit <- Megaparsec.Char.octDigitChar
  digits <- Megaparsec.takeWhileP Nothing (\c -> isOctDigit c || c == '_')
  case readEither ("0o" <> [digit] <> List.filter (/= '_') (Text.unpack digits)) of
    Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
    Right n -> pure n

decLiteral :: Lexer Integer
decLiteral = lexeme do
  digit <- Megaparsec.Char.digitChar
  digits <- Megaparsec.takeWhileP Nothing (\c -> isDigit c || c == '_')
  case readEither ([digit] <> List.filter (/= '_') (Text.unpack digits)) of
    Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
    Right n -> pure n

hexLiteral :: Lexer Integer
hexLiteral = lexeme do
  _ <- Megaparsec.Char.string "0x"
  digit <- Megaparsec.Char.hexDigitChar
  digits <- Megaparsec.takeWhileP Nothing (\c -> isHexDigit c || c == '_')
  case readEither ("0x" <> [digit] <> List.filter (/= '_') (Text.unpack digits)) of
    Left err -> Megaparsec.fancyFailure $ Set.singleton $ Megaparsec.ErrorFail err
    Right n -> pure n

keywordText :: Keyword -> Text
keywordText = \case
  Abort -> "abort"
  Do -> "do"
  Effect -> "effect"
  Else -> "else"
  Enum -> "enum"
  Fn -> "fn"
  For -> "for"
  Handle -> "handle"
  If -> "if"
  Impl -> "impl"
  In -> "in"
  Let -> "let"
  Loop -> "loop"
  Match -> "match"
  Mod -> "mod"
  Mut -> "mut"
  Op -> "op"
  Pub -> "pub"
  Recover -> "recover"
  Resume -> "resume"
  Return -> "return"
  Returning -> "returning"
  Struct -> "struct"
  Trait -> "trait"
  Type -> "type"
  Use -> "use"
  Where -> "where"
  While -> "while"

punctuationText :: Punctuation -> Text
punctuationText = \case
  Assign -> "="
  LessThan -> "<"
  LessThanOrEqual -> "<="
  Equal -> "=="
  NotEqual -> "!="
  GreaterThanOrEqual -> ">="
  GreaterThan -> ">"
  And -> "&&"
  Or -> "||"
  Not -> "!"
  Tilde -> "~"
  Plus -> "+"
  Minus -> "-"
  Star -> "*"
  Slash -> "/"
  Percent -> "%"
  Caret -> "^"
  Ampersand -> "&"
  Pipe -> "|"
  ShiftLeft -> "<<"
  ShiftRight -> ">>"
  PlusAssign -> "+="
  MinusAssign -> "-="
  StarAssign -> "*="
  SlashAssign -> "/="
  PercentAssign -> "%="
  CaretAssign -> "^="
  AmpersandAssign -> "&="
  PipeAssign -> "|="
  ShiftLeftAssign -> "<<="
  ShiftRightAssign -> ">>="
  Inherit -> ":<"
  At -> "@"
  Dot -> "."
  DotDot -> ".."
  DotDotDot -> "..."
  DotDotEqual -> "..="
  Comma -> ","
  Semicolon -> ";"
  Colon -> ":"
  ColonColon -> "::"
  Arrow -> "->"
  LeftArrow -> "<-"
  FatArrow -> "=>"
  Hash -> "#"
  Dollar -> "$"
  Question -> "?"
  Underscore -> "_"
  LeftBrace -> "{"
  RightBrace -> "}"
  LeftBracket -> "["
  RightBracket -> "]"
  LeftParen -> "("
  RightParen -> ")"

spaceConsumer :: Lexer ()
spaceConsumer =
  Megaparsec.Char.Lexer.space
    Megaparsec.Char.space1
    (Megaparsec.Char.Lexer.skipLineComment "//")
    (Megaparsec.Char.Lexer.skipBlockCommentNested "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = Megaparsec.Char.Lexer.lexeme spaceConsumer

mkKeyword :: Keyword -> Lexer Keyword
mkKeyword kw = lexeme do
  _ <- Megaparsec.Char.string (keywordText kw)
  Megaparsec.notFollowedBy alphaNumUnderscoreChar
  pure kw

mkPunctuation :: Punctuation -> Lexer Punctuation
mkPunctuation Underscore = Megaparsec.try $ lexeme do
  Underscore <$ Megaparsec.Char.char '_' <* Megaparsec.notFollowedBy alphaNumUnderscoreChar
mkPunctuation p = lexeme do
  p <$ Megaparsec.Char.string (punctuationText p)

alphaNumUnderscoreChar :: Lexer Char
alphaNumUnderscoreChar = Megaparsec.Char.alphaNumChar <|> Megaparsec.Char.char '_'
