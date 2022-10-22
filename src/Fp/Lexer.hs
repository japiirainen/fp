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
        , Minus <$ symbol "-"
        , Divide <$ symbol "÷"
        ]
        <?> "primitive function"
    , Combinators.choice
        [ Comp <$ (symbol "Comp" <|> symbol "∘")
        , Transpose <$ (symbol "Transpose" <|> symbol "Trans")
        , ApplyToAll <$ (symbol "ApplyToAll" <|> symbol "α")
        , Insert <$ (symbol "Insert" <|> symbol "/")
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
    , Comma <$ symbol ","
    , number
    ]

isLabel0 :: Char -> Bool
isLabel0 = Char.isUpper

isLabel :: Char -> Bool
isLabel c = Char.isAlphaNum c || c == '_' || c == '-' || c == '/'

reserved :: HashSet Text
reserved =
  HashSet.fromList
    [ "T"
    , "F"
    , "Def"
    , "Trans"
    , "Transpose"
    , "ApplyToAll"
    , "Comp"
    ]

label :: Parser Token
label = (lexeme . try) do
  c0 <- Megaparsec.satisfy isLabel0 <?> "label character"
  cs <- Megaparsec.takeWhileP (Just "label character") isLabel
  let result = Text.cons c0 cs
  Monad.guard (not (HashSet.member result reserved))
  return (Label result)

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
  | Plus
  | Times
  | Minus
  | Divide
  | OpenBracket
  | CloseBracket
  | OpenParen
  | CloseParen
  | OpenAngle
  | CloseAngle
  | RealLiteral Scientific
  | Int Int
  | Def
  | Comma
  | Bottom
  | T
  | F
  | Label Text
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
      (Location.renderError "Invalid input - Lexing failed" location)
