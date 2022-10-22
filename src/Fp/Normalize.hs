{-# LANGUAGE OverloadedStrings #-}

module Fp.Normalize where

import Data.Text (Text)
import Fp.Location (Location)
import Fp.Syntax (Syntax)
import Prelude hiding (succ)

import Fp.Input (Input)

evaluate ::
  -- | Surface syntax
  [Syntax Location Input] ->
  -- | Result, free of reducible sub-expressions
  Text
evaluate syntax = "Normalize.evaluate Not implemented!"