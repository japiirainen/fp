{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

{- | This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Fp.Value (
  -- * Value
  Value (..),

  -- * Helpers
  shouldShow,
) where

import Data.Text (Text)
import Fp.Syntax (Atom, Combinator1, Combinator2, Primitive)

{- | This type represents a fully evaluated expression with no reducible
    sub-expressions
    There are two benefits to using a type separate from the surface syntax for
    this purpose:
    * To avoid wastefully reducing the same sub-expression multiple times
    * To use a more efficient representation for reduction purposes
-}
data Value
  = Variable Text
  | Application Value Value
  | List [Value]
  | Construction [Value]
  | Atom Atom
  | Bottom
  | Primitive Primitive
  | Combinator1 Combinator1 Value
  | Combinator2 Combinator2 Value Value
  deriving stock (Eq, Show)

{- | determines weather or not we should show this
     `Value` to the user when the program is ran.
-}
shouldShow :: Value -> Bool
shouldShow = \case
  Variable _ -> True
  List _ -> True
  Atom _ -> True
  Bottom -> True
  Construction _ -> False
  Application _ _ -> False
  Primitive _ -> False
  Combinator1 _ _ -> False
  Combinator2 {} -> False
