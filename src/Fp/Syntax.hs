{-# LANGUAGE DerivingStrategies #-}

module Fp.Syntax (
  -- * Syntax
  Syntax (..),
  Atom (..),
  Object (..),
  Primitive (..),
  Combinator1 (..),
  Combinator2 (..),
) where

import Data.Scientific (Scientific)
import Data.Text (Text)

-- | The surface syntax of the language
data Syntax s a
  = Application {location :: s, function :: Syntax s a, argument :: Syntax s a}
  | Definition {location :: s, name :: Text, body :: Syntax s a}
  | Object {location :: s, object :: Object}
  | Combinator1 {location :: s, c1 :: Combinator1, argument :: Syntax s a}
  | Combinator2 {location :: s, c2 :: Combinator2, argument1 :: Syntax s a, argument2 :: Syntax s a}
  | Primitive {location :: s, primitive :: Primitive}
  deriving stock (Eq, Show)

-- | A scalar value in `fp` language
data Atom
  = Bool Bool
  | Int Int
  | Real Scientific
  | Symbol Text
  deriving stock (Eq, Show)

data Object
  = Bottom
  | AtomObject Atom
  | Seq [Object]
  deriving stock (Eq, Show)

data Combinator1
  = ApplyToAll
  | Insert
  deriving stock (Eq, Show)

data Combinator2
  = Composition
  deriving stock (Eq, Show)

data Primitive
  = Transpose
  | Plus
  | Times
  | Minus
  | Divide
  deriving stock (Eq, Show)