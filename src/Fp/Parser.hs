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

import Control.Applicative (Alternative (many), optional, (<|>))
import Control.Applicative.Combinators (sepBy)
import Data.Functor (void, ($>))
import Data.Scientific (Scientific)
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

matchObjectLabel :: Token -> Maybe Text
matchObjectLabel (Lexer.ObjectLabel l) = Just l
matchObjectLabel _ = Nothing

matchReal :: Token -> Maybe Scientific
matchReal (Lexer.RealLiteral n) = Just n
matchReal _ = Nothing

matchInt :: Token -> Maybe Int
matchInt (Lexer.Int n) = Just n
matchInt _ = Nothing

matchNth :: Token -> Maybe Int
matchNth (Lexer.Nth n) = Just n
matchNth _ = Nothing

matchNthBack :: Token -> Maybe Int
matchNthBack (Lexer.NthBack n) = Just n
matchNthBack _ = Nothing

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

locatedTerminal :: (Token -> Maybe a) -> Parser r (Offset, a)
locatedTerminal match = Earley.terminal match'
  where
    match' locatedToken_@LocatedToken {start} = do
      a <- match (Lexer.token locatedToken_)
      return (start, a)

locatedLabel :: Parser r (Offset, Text)
locatedLabel = locatedTerminal matchLabel

locatedObjectLabel :: Parser r (Offset, Text)
locatedObjectLabel = locatedTerminal matchObjectLabel

locatedReal :: Parser r (Offset, Scientific)
locatedReal = locatedTerminal matchReal

locatedInt :: Parser r (Offset, Int)
locatedInt = locatedTerminal matchInt

locatedNth :: Parser r (Offset, Int)
locatedNth = locatedTerminal matchNth

locatedNthBack :: Parser r (Offset, Int)
locatedNthBack = locatedTerminal matchNthBack

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
  Lexer.ApplyToAll -> "applyToALl"
  Lexer.Insert -> "insert"
  Lexer.Comp -> "comp"
  Lexer.Bottom -> "bottom"
  Lexer.Label _ -> "a label"
  Lexer.ObjectLabel _ -> "a object label"
  Lexer.Transpose -> "transpose"
  Lexer.Atom -> "atom"
  Lexer.Eq -> "eq"
  Lexer.Null -> "null"
  Lexer.Reverse -> "reverse"
  Lexer.Distl -> "distl"
  Lexer.Distr -> "distr"
  Lexer.Length -> "length"
  Lexer.Id -> "id"
  Lexer.NthBack n -> Text.pack (show n) <> "~"
  Lexer.Nth n -> "~" <> Text.pack (show n)
  Lexer.CloseAngle -> ">"
  Lexer.OpenAngle -> "<"
  Lexer.CloseParen -> ")"
  Lexer.OpenParen -> "("
  Lexer.CloseBracket -> "]"
  Lexer.OpenBracket -> "["
  Lexer.Comma -> ","
  Lexer.Colon -> ":"
  Lexer.SemiColon -> ";"
  Lexer.Plus -> "+"
  Lexer.Times -> "*"
  Lexer.Divide -> "/"
  Lexer.Dash -> "-"
  Lexer.UnderScore -> "_"
  Lexer.RealLiteral _ -> "a real literal"
  Lexer.Int _ -> "an integer"
  Lexer.Def -> "Def"
  Lexer.EmptySeq -> "⌽"
  Lexer.Arrow -> "→"
  Lexer.And -> "∧"
  Lexer.Not -> "¬"
  Lexer.Or -> "∨"

grammar :: Grammar r (Parser r [Syntax Offset Input])
grammar = mdo
  expression <-
    rule
      ( do
          location <- locatedToken Lexer.Def
          name <- label
          token Lexer.Equals
          body <- primitiveExpression <|> expression
          pure Syntax.Definition {..}
          <|> do
            predicate <- primitiveExpression <|> expression
            token Lexer.Arrow
            ifTrue <- primitiveExpression <|> expression
            token Lexer.SemiColon
            ifFalse <- primitiveExpression <|> expression
            pure Syntax.If {location = Syntax.location predicate, ..}
          <|> do
            function <- primitiveExpression <|> expression
            token Lexer.Colon
            argument <- primitiveExpression <|> expression
            pure Syntax.Application {location = Syntax.location function, ..}
          <|> do
            token Lexer.OpenParen
            e <- expression
            token Lexer.CloseParen
            return e
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
          location <- locatedToken Lexer.OpenAngle
          optional (token Lexer.Comma)
          elements <- primitiveExpression `sepBy` token Lexer.Comma
          optional (token Lexer.Comma)
          token Lexer.CloseAngle
          pure Syntax.List {..}
          <|> do
            location <- locatedToken Lexer.OpenBracket
            optional (token Lexer.Comma)
            functions <-
              (compExpression <|> primitiveExpression)
                `sepBy` token Lexer.Comma
            optional (token Lexer.Comma)
            token Lexer.CloseBracket
            pure Syntax.Construction {..}
          <|> do
            location <- locatedToken Lexer.EmptySeq
            pure Syntax.List {elements = [], ..}
          <|> do
            location <- locatedToken Lexer.T
            pure Syntax.Atom {atom = Syntax.Bool True, ..}
          <|> do
            location <- locatedToken Lexer.F
            pure Syntax.Atom {atom = Syntax.Bool False, ..}
          <|> do
            ~(location, name) <- locatedObjectLabel
            pure Syntax.Atom {atom = Syntax.Symbol name, ..}
          <|> do
            ~(location, name) <- locatedLabel
            pure Syntax.Variable {name = name, ..}
          <|> do
            sign <- (token Lexer.Dash $> negate) <|> pure id
            ~(location, n) <- locatedReal
            pure Syntax.Atom {atom = Syntax.Real (sign n), ..}
          <|> do
            token Lexer.Dash
            ~(location, n) <- locatedInt
            pure Syntax.Atom {atom = Syntax.Int (fromIntegral (negate n)), ..}
          <|> do
            ~(location, n) <- locatedInt
            pure Syntax.Atom {atom = Syntax.Int (fromIntegral n), ..}
          <|> do
            location <- locatedToken Lexer.Bottom
            pure Syntax.Bottom {..}
          <|> do
            location <- locatedToken Lexer.Transpose
            pure Syntax.Primitive {primitive = Syntax.Transpose, ..}
          <|> do
            location <- locatedToken Lexer.Atom
            pure Syntax.Primitive {primitive = Syntax.AtomP, ..}
          <|> do
            location <- locatedToken Lexer.Eq
            pure Syntax.Primitive {primitive = Syntax.Eq, ..}
          <|> do
            location <- locatedToken Lexer.Null
            pure Syntax.Primitive {primitive = Syntax.Null, ..}
          <|> do
            location <- locatedToken Lexer.Reverse
            pure Syntax.Primitive {primitive = Syntax.Reverse, ..}
          <|> do
            location <- locatedToken Lexer.Distl
            pure Syntax.Primitive {primitive = Syntax.Distl, ..}
          <|> do
            ~(location, n) <- locatedNth
            pure Syntax.Primitive {primitive = Syntax.Nth n, ..}
          <|> do
            ~(location, n) <- locatedNthBack
            pure Syntax.Primitive {primitive = Syntax.NthBack n, ..}
          <|> do
            location <- locatedToken Lexer.Distr
            pure Syntax.Primitive {primitive = Syntax.Distr, ..}
          <|> do
            location <- locatedToken Lexer.Length
            pure Syntax.Primitive {primitive = Syntax.Length, ..}
          <|> do
            location <- locatedToken Lexer.Id
            pure Syntax.Primitive {primitive = Syntax.Id, ..}
          <|> do
            location <- locatedToken Lexer.Plus
            pure Syntax.Primitive {primitive = Syntax.Plus, ..}
          <|> do
            location <- locatedToken Lexer.Times
            pure Syntax.Primitive {primitive = Syntax.Times, ..}
          <|> do
            location <- locatedToken Lexer.Dash
            pure Syntax.Primitive {primitive = Syntax.Minus, ..}
          <|> do
            location <- locatedToken Lexer.Divide
            pure Syntax.Primitive {primitive = Syntax.Divide, ..}
          <|> do
            location <- locatedToken Lexer.Not
            pure Syntax.Primitive {primitive = Syntax.Not, ..}
          <|> do
            location <- locatedToken Lexer.And
            pure Syntax.Primitive {primitive = Syntax.And, ..}
          <|> do
            location <- locatedToken Lexer.Or
            pure Syntax.Primitive {primitive = Syntax.Or, ..}
          <|> do
            location <- locatedToken Lexer.Insert
            argument <- primitiveExpression
            pure Syntax.Combinator1 {c1 = Syntax.Insert, ..}
          <|> do
            location <- locatedToken Lexer.ApplyToAll
            argument <- primitiveExpression
            pure Syntax.Combinator1 {c1 = Syntax.ApplyToAll, ..}
          <|> do
            location <- locatedToken Lexer.UnderScore
            argument <- primitiveExpression
            pure Syntax.Combinator1 {c1 = Syntax.Const, ..}
          <|> do
            token Lexer.OpenParen
            e <- primitiveExpression
            token Lexer.CloseParen
            return e
      )

  return (many expression)

parse ::
  -- | Name of the input (used for error messages)
  String ->
  -- | Source code
  Text ->
  Either ParseError [Syntax Offset Input]
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
