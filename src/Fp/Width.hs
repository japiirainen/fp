{-# LANGUAGE NamedFieldPuns #-}

module Fp.Width (
  -- * Width
  getWidth,
  defaultWidth,
) where

import System.Console.Terminal.Size (Window (..))

import qualified System.Console.Terminal.Size as Size

-- | Get the width of the terminal (in columns).
getWidth :: IO Int
getWidth = do
  maybeWindow <- Size.size
  pure $ case maybeWindow of
    Nothing -> defaultWidth
    Just Window {width} -> width

-- | The default width to use
defaultWidth :: Int
defaultWidth = 80