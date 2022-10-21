{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

-- | This module contains the logic for parsing fp files using @Earley@.
module Fp.Parser () where

import Data.Text (Text)
import Fp.Input (Input)
import Fp.Lexer (LocatedToken (..), ParseError (..), Token)
import Fp.Location (Location (..), Offset (..))
import Fp.Syntax (Syntax)
import Text.Earley (Grammar, Prod, Report (..), rule, (<?>))

import qualified Data.Text as Text
import qualified Fp.Lexer as Lexer
import qualified Text.Earley as Earley

type Parser r = Prod r Text LocatedToken

matchLabel :: Token -> Maybe Text
matchLabel (Lexer.Label l) = Just l
matchLabel _ = Nothing

grammar :: Grammar r (Parser r (Syntax Offset Input))
grammar = mdo
  undefined

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