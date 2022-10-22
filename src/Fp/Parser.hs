{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

-- | This module contains the logic for parsing fp files using @Earley@.
module Fp.Parser (
  -- * Parsing
  parse,

  -- * Errors related to parsing
  ParseError (..),
) where

import Control.Applicative (Alternative (many), (<|>))
import Data.Functor (void)
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
          body <- primitiveExpression <|> expression
          pure Syntax.Definition {..}
          <|> compExpression
      )

  let comp token_ c2 subExpression = do
        let snoc argument1 (operatorLocation, argument2) =
              Syntax.Combinator2 {location = Syntax.location argument1, ..}

        e0 <- subExpression

        ses <- many do
          s <- locatedToken token_
          e <- subExpression
          return (s, e)

        return (foldl snoc e0 ses)

  compExpression <- rule (comp Lexer.Comp Syntax.Composition primitiveExpression)

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
            token Lexer.OpenParen
            e <- primitiveExpression
            token Lexer.CloseParen
            return e
      )

  return expression

-- parse "" "(/+)∘(α*)∘Trans"
-- Right (Combinator2 {location = 1, argument1 = Combinator2 {location = 1, argument1 = Combinator1 {location = 1, c1 = Insert, argument = Primitive {location = 2, primitive = Plus}}, operatorLocation = 4, c2 = Composition, argument2 = Combinator1 {location = 6, c1 = ApplyToAll, argument = Primitive {location = 7, primitive = Times}}}, operatorLocation = 9, c2 = Composition, argument2 = Primitive {location = 10, primitive = Transpose}})
-- Right (Combinator2 {location = 10, argument1 = Combinator2 {location = 10, argument1 = Combinator1 {location = 10, c1 = Insert, argument = Primitive {location = 11, primitive = Plus}}, operatorLocation = 13, c2 = Composition, argument2 = Combinator1 {location = 15, c1 = ApplyToAll, argument = Primitive {location = 16, primitive = Times}}}, operatorLocation = 18, c2 = Composition, argument2 = Primitive {location = 19, primitive = Transpose}})

-- >>> parse "" "Def Foobar = /-"
-- Right (Definition {location = 0, name = "Foobar", body = Combinator1 {location = 13, c1 = Insert, argument = Primitive {location = 14, primitive = Minus}}})

-- >>> parse "" "Def IP = (/+)∘(α*)∘Trans"
-- Right (Definition {location = 0, name = "IP", body = Combinator2 {location = 10, argument1 = Combinator2 {location = 10, argument1 = Combinator1 {location = 10, c1 = Insert, argument = Primitive {location = 11, primitive = Plus}}, operatorLocation = 13, c2 = Composition, argument2 = Combinator1 {location = 15, c1 = ApplyToAll, argument = Primitive {location = 16, primitive = Times}}}, operatorLocation = 18, c2 = Composition, argument2 = Primitive {location = 19, primitive = Transpose}}})
--

-- >>> 1 + 1
-- 2
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
