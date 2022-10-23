{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Fp.Syntax (
  -- * Syntax
  Syntax (..),
  Atom (..),
  Primitive (..),
  Combinator1 (..),
  Combinator2 (..),
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Scientific (Scientific)
import Data.Text (Text)

-- | The surface syntax of the language
data Syntax s a
  = Application {location :: s, function :: Syntax s a, argument :: Syntax s a}
  | Definition {location :: s, name :: Text, body :: Syntax s a}
  | Bottom {location :: s}
  | Atom {location :: s, atom :: Atom}
  | List
      {location :: s, elements :: [Syntax s a]}
  | Combinator1
      {location :: s, c1 :: Combinator1, argument :: Syntax s a}
  | Combinator2 {location :: s, argument1 :: Syntax s a, operatorLocation :: s, c2 :: Combinator2, argument2 :: Syntax s a}
  | Primitive {location :: s, primitive :: Primitive}
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance Bifunctor Syntax where
  first f Application {..} =
    Application {location = f location, function = first f function, argument = first f argument, ..}
  first f Definition {..} =
    Definition {location = f location, body = first f body, ..}
  first f List {..} =
    List {location = f location, elements = fmap (first f) elements, ..}
  first f Atom {..} =
    Atom {location = f location, ..}
  first f Bottom {..} =
    Bottom {location = f location, ..}
  first f Combinator1 {..} =
    Combinator1 {location = f location, argument = first f argument, ..}
  first f Combinator2 {..} =
    Combinator2 {location = f location, argument1 = first f argument1, operatorLocation = f operatorLocation, argument2 = first f argument2, ..}
  first f Primitive {..} =
    Primitive {location = f location, ..}

  second = fmap

-- | A scalar value in `fp` language
data Atom
  = Bool Bool
  | Int Int
  | Real Scientific
  | Symbol Text
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
