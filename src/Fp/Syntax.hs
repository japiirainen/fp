{-# LANGUAGE DerivingStrategies #-}

module Fp.Syntax (
  -- * Syntax
  Syntax (..),
  Atom (..),
  Object (..),
  Function (..),
) where

import Data.Scientific (Scientific)
import Data.Text (Text)

-- | The surface syntax of the language
data Syntax s a
  = Application {location :: s, function :: Syntax s a, argument :: Syntax s a}
  | Definition {location :: s, name :: Text, body :: Syntax s a}
  | Object {location :: s, object :: Object}
  | Function {location :: s, f :: Function}
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

{- | A 'function' in the `fp` language can be either
 an `functional-form` or a `primitive-function`
-}
data Function
  = Transpose
  | Composition
  | ApplyToAll
  | Insert
  deriving stock (Eq, Show)