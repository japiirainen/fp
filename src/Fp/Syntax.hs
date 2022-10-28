{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Scientific (Scientific (..))
import Data.Text (Text)
import Fp.Pretty (Pretty (..), keyword, label, punctuation)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.String (IsString (..))
import qualified Data.Text as Text
import qualified Fp.Pretty as Pretty
import qualified Prettyprinter as Pretty

-- | The surface syntax of the language
data Syntax s a
  = Variable {location :: s, name :: Text}
  | Application {location :: s, function :: Syntax s a, argument :: Syntax s a}
  | Definition {location :: s, name :: Text, body :: Syntax s a}
  | Bottom {location :: s}
  | Atom {location :: s, atom :: Atom}
  | List {location :: s, elements :: [Syntax s a]}
  | If {location :: s, predicate :: Syntax s a, ifTrue :: Syntax s a, ifFalse :: Syntax s a}
  | Construction {location :: s, functions :: [Syntax s a]}
  | Combinator1 {location :: s, c1 :: Combinator1, argument :: Syntax s a}
  | Combinator2 {location :: s, argument1 :: Syntax s a, operatorLocation :: s, c2 :: Combinator2, argument2 :: Syntax s a}
  | Primitive {location :: s, primitive :: Primitive}
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance Bifunctor Syntax where
  first f Variable {..} =
    Variable {location = f location, ..}
  first f Application {..} =
    Application {location = f location, function = first f function, argument = first f argument, ..}
  first f Definition {..} =
    Definition {location = f location, body = first f body, ..}
  first f List {..} =
    List {location = f location, elements = fmap (first f) elements, ..}
  first f Construction {..} =
    Construction {location = f location, functions = fmap (first f) functions, ..}
  first f Atom {..} =
    Atom {location = f location, ..}
  first f Bottom {..} =
    Bottom {location = f location, ..}
  first f If {..} =
    If
      { location = f location
      , predicate = first f predicate
      , ifTrue = first f ifTrue
      , ifFalse = first f ifFalse
      , ..
      }
  first f Combinator1 {..} =
    Combinator1 {location = f location, argument = first f argument, ..}
  first f Combinator2 {..} =
    Combinator2 {location = f location, argument1 = first f argument1, operatorLocation = f operatorLocation, argument2 = first f argument2, ..}
  first f Primitive {..} =
    Primitive {location = f location, ..}

  second = fmap

instance IsString (Syntax () a) where
  fromString string =
    Variable {location = (), name = fromString string}

instance Pretty a => Pretty (Syntax s a) where
  pretty = prettyExpression

-- | A scalar value in `fp` language
data Atom
  = Bool Bool
  | Int Int
  | Real Scientific
  | Symbol Text
  deriving stock (Eq, Show)

instance Pretty Atom where
  pretty (Bool True) = Pretty.scalar "T"
  pretty (Bool False) = Pretty.scalar "F"
  pretty (Int n) = Pretty.scalar (pretty n)
  pretty (Real n) = Pretty.scalar (pretty n)
  pretty (Symbol name) = Pretty.scalar (pretty name)

data Combinator1
  = ApplyToAll
  | Insert
  | Const
  deriving stock (Eq, Show)

instance Pretty Combinator1 where
  pretty ApplyToAll = Pretty.operator "α"
  pretty Insert = Pretty.operator "/"
  pretty Const = Pretty.operator "_"

data Combinator2
  = Composition
  deriving stock (Eq, Show)

instance Pretty Combinator2 where
  pretty Composition = Pretty.operator "∘"

data Primitive
  = Transpose
  | Plus
  | Times
  | Minus
  | Divide
  | AtomP
  | Eq
  | Null
  | Reverse
  | Distl
  | Distr
  | Length
  | Id
  | Nth Int
  deriving stock (Eq, Show)

instance Pretty Primitive where
  pretty = \case
    Transpose -> Pretty.builtin "⍉"
    Plus -> Pretty.builtin "+"
    Times -> Pretty.builtin "*"
    Minus -> Pretty.builtin "-"
    Divide -> Pretty.builtin "÷"
    AtomP -> Pretty.builtin "atom"
    Eq -> Pretty.builtin "eq"
    Null -> Pretty.builtin "null"
    Reverse -> Pretty.builtin "reverse"
    Distl -> Pretty.builtin "distl"
    Distr -> Pretty.builtin "distr"
    Length -> Pretty.builtin "length"
    Id -> Pretty.builtin "id"
    Nth n -> Pretty.builtin (pretty n)

-- | Pretty-print a @Text@ literal
prettyTextLiteral :: Text -> Doc AnsiStyle
prettyTextLiteral text =
  "\""
    <> ( pretty
          . Text.replace "\"" "\\\""
          . Text.replace "\b" "\\b"
          . Text.replace "\f" "\\f"
          . Text.replace "\n" "\\n"
          . Text.replace "\r" "\\r"
          . Text.replace "\t" "\\t"
          . Text.replace "\\" "\\\\"
       )
      text
    <> "\""

prettyExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyExpression Variable {..} = label (pretty name)
prettyExpression Primitive {..} = label (pretty primitive)
prettyExpression Atom {..} = label (pretty atom)
prettyExpression Bottom {} = label (Pretty.scalar "⊥")
prettyExpression List {elements = []} = punctuation "⌽"
prettyExpression List {elements = (element : elements)} =
  Pretty.group (Pretty.flatAlt long short)
  where
    short =
      punctuation "<"
        <> " "
        <> prettyExpression element
        <> foldMap (\e -> punctuation "," <> " " <> prettyExpression e) elements
        <> " "
        <> punctuation ">"

    long =
      Pretty.align
        ( "< "
            <> prettyLongElement element
            <> foldMap (\e -> punctuation "," <> " " <> prettyLongElement e) elements
            <> ">"
        )
    prettyLongElement e = prettyExpression e <> Pretty.hardline
prettyExpression Construction {functions = []} =
  punctuation "[" <> " " <> punctuation "]"
prettyExpression Construction {functions = (fn : fns)} =
  Pretty.group (Pretty.flatAlt long short)
  where
    short =
      punctuation "["
        <> " "
        <> prettyExpression fn
        <> foldMap (\e -> punctuation "," <> " " <> prettyExpression e) fns
        <> " "
        <> punctuation "]"

    long =
      Pretty.align
        ( "[ "
            <> prettyLongElement fn
            <> foldMap (\e -> punctuation "," <> " " <> prettyLongElement e) fns
            <> "]"
        )
    prettyLongElement e = prettyExpression e <> Pretty.hardline
prettyExpression Combinator1 {..} = prettyCombinator1 c1 argument
prettyExpression Application {..} = prettyApplication function argument
prettyExpression Definition {..} = Pretty.group (Pretty.flatAlt long short)
  where
    short =
      keyword "Def"
        <> " "
        <> prettyTextLiteral name
        <> " "
        <> keyword "≡"
        <> " "
        <> prettyExpression body

    long =
      Pretty.align
        ( keyword "Def"
            <> " "
            <> prettyTextLiteral name
            <> Pretty.hardline
            <> Pretty.hardline
            <> " "
            <> keyword "≡"
            <> "  "
            <> prettyExpression body
        )
prettyExpression If {..} =
  Pretty.group (Pretty.flatAlt long short)
  where
    short =
      prettyExpression predicate
        <> keyword " → "
        <> prettyExpression ifTrue
        <> keyword ";"
        <> " "
        <> prettyExpression ifFalse

    long =
      Pretty.align
        ( prettyExpression predicate
            <> Pretty.hardline
            <> keyword " → "
            <> prettyExpression ifTrue
            <> Pretty.hardline
            <> keyword ":"
            <> " "
            <> prettyExpression ifFalse
        )
prettyExpression other = prettyComposition other

prettyCombinator1 :: Pretty a => Combinator1 -> Syntax s a -> Doc AnsiStyle
prettyCombinator1 c1 argument = Pretty.group (Pretty.flatAlt long short)
  where
    short =
      "("
        <> pretty c1
        <> " "
        <> prettyExpression argument
        <> ")"

    long =
      Pretty.align
        ( "("
            <> pretty c1
            <> " "
            <> Pretty.hardline
            <> Pretty.hardline
            <> prettyExpression argument
            <> ")"
        )

prettyApplication :: Pretty a => Syntax s a -> Syntax s a -> Doc AnsiStyle
prettyApplication function argument = Pretty.group (Pretty.flatAlt long short)
  where
    short =
      pretty function
        <> " "
        <> Pretty.operator ":"
        <> " "
        <> prettyExpression argument

    long =
      Pretty.align
        ( pretty function
            <> " "
            <> Pretty.hardline
            <> Pretty.hardline
            <> Pretty.operator ":"
            <> " "
            <> prettyExpression argument
        )

prettyCombinator2 ::
  Pretty a =>
  Combinator2 ->
  (Syntax s a -> Doc AnsiStyle) ->
  (Syntax s a -> Doc AnsiStyle)
prettyCombinator2 operator0 prettyNext expression@Combinator2 {c2 = operator1}
  | operator0 == operator1 = Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Combinator2 {..}
      | operator0 == c2 =
          prettyShort argument1
            <> " "
            <> pretty c2
            <> " "
            <> prettyNext argument2
    prettyShort other =
      prettyNext other

    prettyLong Combinator2 {..}
      | operator0 == c2 =
          prettyLong argument1
            <> Pretty.hardline
            <> pretty c2
            <> pretty (Text.replicate spacing " ")
            <> prettyNext argument2
    prettyLong other =
      pretty (Text.replicate indent " ")
        <> prettyNext other

    operatorWidth = Text.length (Pretty.toText operator0)

    alignment = 2

    align n = ((n `div` alignment) + 1) * alignment

    indent = align operatorWidth

    spacing = indent - operatorWidth
prettyCombinator2 _ prettyNext other =
  prettyNext other

prettyComposition :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyComposition = prettyCombinator2 Composition prettyExpression
