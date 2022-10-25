{-# LANGUAGE DerivingStrategies #-}

{- | This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Fp.Value (
  -- * Value
  Value (..),
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
  | Atom Atom
  | Bottom
  | Primitive Primitive
  | Combinator1 Combinator1 Value
  | Combinator2 Combinator2 Value Value
  deriving stock (Eq, Show)
