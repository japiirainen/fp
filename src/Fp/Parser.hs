{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

-- | This module contains the logic for parsing fp files using @Earley@.
module Fp.Parser () where

import Control.Applicative ((<|>))
import Data.Functor (void, ($>))
import Data.Text (Text)
import Fp.Input (Input)
import Fp.Lexer (LocatedToken (LocatedToken), ParseError (..), Token)
import Fp.Location (Location (..), Offset (..))
import Fp.Syntax (Syntax)
import Text.Earley (Grammar, Prod, Report (..), rule, (<?>))

import qualified Data.Text as Text
import qualified Fp.Lexer as Lexer
import qualified Fp.Syntax as Syntax
import qualified Text.Earley as Earley

type Parser r = Prod r Text LocatedToken

matchLabel :: Token -> Maybe Text
matchLabel (Lexer.Label l) = Just l
matchLabel _ = Nothing

terminal :: (Token -> Maybe a) -> Parser r a
terminal match = Earley.terminal match'
  where
    match' locatedToken_ = match (Lexer.token locatedToken_)

label :: Parser r Text
label = terminal matchLabel

token :: Token -> Parser r ()
token t = void (Earley.satisfy predicate <?> render t)
  where
    predicate locatedToken_ = Lexer.token locatedToken_ == t

locatedToken :: Token -> Parser r Offset
locatedToken expected =
  Earley.terminal capture <?> render expected
  where
    capture LocatedToken {Lexer.token = actual, ..}
      | expected == actual = Just start
      | otherwise = Nothing

render :: Token -> Text
render = \case
  Lexer.F -> "F"
  Lexer.T -> "T"
  Lexer.Equals -> "="
  Lexer.ApplyToAll -> "ApplyToAll"
  Lexer.Insert -> "Insert"
  Lexer.Comp -> "Comp"
  Lexer.Bottom -> "Bottom"
  Lexer.Label _ -> "a label"
  Lexer.Transpose -> "Transpose"
  Lexer.CloseAngle -> ">"
  Lexer.OpenAngle -> "<"
  Lexer.CloseParen -> ")"
  Lexer.OpenParen -> "("
  Lexer.CloseBracket -> "]"
  Lexer.OpenBracket -> "["
  Lexer.Comma -> ","
  Lexer.Plus -> "+"
  Lexer.Times -> "*"
  Lexer.Minus -> "-"
  Lexer.Divide -> "/"
  Lexer.RealLiteral _ -> "a real literal"
  Lexer.Int _ -> "an integer"
  Lexer.Def -> "Def"

grammar :: Grammar r (Parser r (Syntax Offset Input))
grammar = mdo
  expression <-
    rule
      ( do
          location <- locatedToken Lexer.Def
          name <- label
          token Lexer.Equals
          body <- primitiveExpression
          pure Syntax.Definition {..}
      )
  primitiveExpression <-
    rule
      ( do
          location <- locatedToken Lexer.Transpose
          pure Syntax.Primitive {primitive = Syntax.Transpose, ..}
          <|> do
            location <- locatedToken Lexer.Plus
            pure Syntax.Primitive {primitive = Syntax.Plus, ..}
          <|> do
            location <- locatedToken Lexer.Times
            pure Syntax.Primitive {primitive = Syntax.Times, ..}
          <|> do
            location <- locatedToken Lexer.Minus
            pure Syntax.Primitive {primitive = Syntax.Minus, ..}
          <|> do
            location <- locatedToken Lexer.Divide
            pure Syntax.Primitive {primitive = Syntax.Divide, ..}
          <|> do
            location <- locatedToken Lexer.Insert
            argument <- primitiveExpression
            pure Syntax.Combinator1 {c1 = Syntax.Insert, ..}
          <|> do
            location <- locatedToken Lexer.ApplyToAll
            argument <- primitiveExpression
            pure Syntax.Combinator1 {c1 = Syntax.ApplyToAll, ..}
          <|> do
            argument1 <- primitiveExpression
            location <- locatedToken Lexer.Comp
            argument2 <- primitiveExpression
            pure Syntax.Combinator2 {c2 = Syntax.Composition, ..}
          <|> do
            token Lexer.OpenParen
            e <- primitiveExpression
            token Lexer.CloseParen
            return e
      )

  return expression

-- | Parse a complete expression
parse ::
  -- | Name of the input (used for error messages)
  String ->
  -- | Source code
  Text ->
  Either ParseError (Syntax Offset Input)
parse name code = do
  tokens <- Lexer.lex name code

  case Earley.fullParses (Earley.parser grammar) tokens of
    ([], Report {..}) -> do
      let offset =
            case unconsumed of
              [] -> Offset (Text.length code)
              locatedToken_ : _ -> Lexer.start locatedToken_

      Left (ParsingFailed (Location {..}))
    (result : _, _) -> do
      return result
