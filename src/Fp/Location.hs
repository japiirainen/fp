{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | This module contains the `Location` type, which is used to for attaching
    source locations to error messages.
-}
module Fp.Location (
  -- * Location
  Location (..),
  Offset (..),
  renderError,
) where

import Data.Text (Text)
import Text.Megaparsec (PosState (..), SourcePos (..))

import qualified Data.Text as Text
import qualified Text.Megaparsec.Pos as Pos
import qualified Text.Megaparsec.Stream as Stream

-- | Offsets are stored in characters (0-indexed)
newtype Offset = Offset {getOffset :: Int}
  deriving newtype (Eq, Num, Show)

data Location = Location
  { name :: String
  -- ^ The file or name describing where the code came from
  , code :: Text
  -- ^ The original source code (the entire file)
  -- NOTE:
  -- This will not always be the same for each `Location` because
  -- different subexpressions might originate from different files if
  -- they were imported.
  , offset :: Offset
  -- ^ The offset within the code (in characters).
  }
  deriving stock (Eq, Show)

-- | Render an error message, given a `Location` for the error.
renderError :: Text -> Location -> Text
renderError message Location {..} = prefix <> "\n" <> suffix
  where
    initialState =
      PosState
        { pstateInput = code
        , pstateOffset = 0
        , pstateSourcePos = Pos.initialPos name
        , pstateTabWidth = Pos.defaultTabWidth
        , pstateLinePrefix = ""
        }
    (h, state) = Stream.reachOffset (getOffset offset) initialState
    pos = pstateSourcePos state
    line = Pos.unPos (sourceLine pos)
    column = Pos.unPos (sourceColumn pos)
    suffix =
      case h of
        Just string ->
          let lineText = Text.pack (show line)
              inner = lineText <> " |"
              outer = Text.replicate (Text.length lineText) " " <> " |"
              caret = Text.replicate (column - 1) " " <> "↑"
           in outer
                <> "\n\
                   \"
                <> inner
                <> " "
                <> Text.pack string
                <> "\n\
                   \"
                <> outer
                <> " "
                <> caret
        Nothing -> ""
    prefix =
      Text.pack name
        <> ":"
        <> Text.pack (show line)
        <> ":"
        <> Text.pack (show column)
        <> ": "
        <> message
