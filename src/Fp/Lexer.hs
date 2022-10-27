{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the logic for lexing `fp` programs.
module Fp.Lexer (
  -- * Lexer
  Token (..),
  LocatedToken (..),
  lex,
  reserved,

  -- * Errors related to parsing
  ParseError (..),
) where

import Control.Applicative (Alternative (..))
import Control.Exception.Safe (Exception (..))
import Control.Monad.Combinators (manyTill)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import Fp.Location (Location (..), Offset (..))
import Text.Megaparsec (ParseErrorBundle (..), try, (<?>))
import Prelude hiding (lex)

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Fp.Location as Location
import qualified Text.Megaparsec as Error
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space = Lexer.space Megaparsec.Char.space1 (Lexer.skipLineComment "--") (Lexer.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

parseToken :: Parser Token
parseToken =
  Combinators.choice
    [ label
    , Combinators.choice
        [ Def <$ symbol "Def"
        , Equals <$ (symbol "=" <|> symbol "≡")
        ]
        <?> "keyword"
    , Combinators.choice
        [ Plus <$ symbol "+"
        , Times <$ symbol "*"
        , Divide <$ symbol "÷"
        , Transpose <$ (symbol "transpose" <|> symbol "⍉")
        , Atom <$ symbol "atom"
        , Eq <$ symbol "eq"
        , Null <$ symbol "null"
        , Reverse <$ symbol "reverse"
        , Distl <$ symbol "distl"
        , Distr <$ symbol "distr"
        , Length <$ symbol "length"
        , Id <$ symbol "id"
        ]
        <?> "primitive function"
    , Combinators.choice
        [ Comp <$ (symbol "." <|> symbol "∘")
        , ApplyToAll <$ (symbol "applyToAll" <|> symbol "α")
        , Insert <$ (symbol "insert" <|> symbol "/")
        ]
        <?> "functional form"
    , Combinators.choice
        [ Bottom <$ symbol "⊥"
        , T <$ symbol "T"
        , F <$ symbol "F"
        ]
        <?> "built-in value"
    , OpenBracket <$ symbol "["
    , CloseBracket <$ symbol "]"
    , OpenParen <$ symbol "("
    , CloseParen <$ symbol ")"
    , OpenAngle <$ symbol "<"
    , CloseAngle <$ symbol ">"
    , EmptySeq <$ symbol "⌽"
    , Comma <$ symbol ","
    , Dash <$ symbol "-"
    , Colon <$ symbol ":"
    , number
    ]

isLabel0 :: Char -> Bool
isLabel0 = Char.isAlpha

isLabel :: Char -> Bool
isLabel c = Char.isAlphaNum c || c == '_' || c == '-' || c == '/'

reserved :: HashSet Text
reserved =
  HashSet.fromList
    [ "T"
    , "F"
    , "Def"
    , "transpose"
    , "applyToAll"
    , "insert"
    , "atom"
    , "eq"
    , "null"
    , "reverse"
    , "distl"
    , "distr"
    , "length"
    , "id"
    , "."
    , "α"
    , "+"
    , "*"
    ]

label :: Parser Token
label = (lexeme . try) do
  c0 <- Megaparsec.satisfy isLabel0 <?> "label character"
  cs <- Megaparsec.takeWhileP (Just "label character") isLabel
  let result = Text.cons c0 cs
  Monad.guard (not (HashSet.member result reserved))
  if Text.toUpper result == result
    then return (ObjectLabel result)
    else return (Label result)

parseLocatedToken :: Parser LocatedToken
parseLocatedToken = do
  start <- Offset <$> Megaparsec.getOffset
  token <- parseToken
  return LocatedToken {..}

parseLocatedTokens :: Parser [LocatedToken]
parseLocatedTokens = space >> manyTill parseLocatedToken Megaparsec.eof

-- | Lex a complete expression
lex ::
  String ->
  Text ->
  Either ParseError [LocatedToken]
lex name code =
  case Megaparsec.parse parseLocatedTokens name code of
    Left ParseErrorBundle {..} -> do
      let bundleError :| _ = bundleErrors
          offset = Offset (Error.errorOffset bundleError)
      Left (LexingFailed (Location {..}))
    Right tokens -> return tokens

number :: Parser Token
number = do
  scientific <- lexeme Lexer.scientific

  case Scientific.toBoundedInteger scientific of
    Nothing -> return (RealLiteral scientific)
    Just int -> return (Int int)

-- | Tokens produced by lexing
data Token
  = Equals
  | Comp
  | Transpose
  | ApplyToAll
  | Insert
  | Atom
  | Eq
  | Null
  | Distl
  | Distr
  | Reverse
  | Length
  | Id
  | Plus
  | Times
  | Divide
  | OpenBracket
  | CloseBracket
  | OpenParen
  | CloseParen
  | OpenAngle
  | CloseAngle
  | EmptySeq
  | Dash
  | Colon
  | RealLiteral Scientific
  | Int Int
  | Def
  | Comma
  | Bottom
  | T
  | F
  | --  | Object label is a label that consists
    -- of only upper-case letters excluding 'T' and 'F'.
    ObjectLabel Text
  | --  | Label is a label that consists of lower and upper-case
    -- letters.
    Label Text
  deriving stock (Eq, Show)

{- | A token with offset information attached,
 used for reporting line and column numbers in error messages
-}
data LocatedToken = LocatedToken {token :: Token, start :: Offset}
  deriving stock (Show)

-- | Errors related to lexing and parsing
data ParseError
  = LexingFailed Location
  | ParsingFailed Location
  deriving stock (Eq, Show)

instance Exception ParseError where
  displayException (LexingFailed location) =
    Text.unpack
      (Location.renderError "Invalid input - Lexing failed" location)
  displayException (ParsingFailed location) =
    Text.unpack
      (Location.renderError "Invalid input - Parsing failed" location)
