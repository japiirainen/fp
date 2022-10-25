{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module implements the main interpretation function
module Fp.Interpret (
  -- * Interpret
  Input (..),
  interpret,

  -- * Errors related to interpretation
  InterpretError (..),
) where

import Control.Exception.Safe (Exception (..), Handler (..))
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Fp.Input (Input (..))
import Fp.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Fp.Import as Import
import qualified Fp.Normalize as Normalize
import qualified Fp.Parser as Parser

{- | Interpret `fp` source code, return the inferred type and the evaluated
    result
    This is the top-level function for the `fp` interpreter
-}
interpret ::
  (MonadError InterpretError m, MonadIO m) =>
  Input ->
  m Value
interpret input = do
  eitherPartiallyResolved <- do
    liftIO
      ( Exception.catches
          (fmap Right (Import.resolve input))
          [ Handler (return . Left . ParseError)
          , Handler (return . Left . ImportError)
          ]
      )

  resolved <- case eitherPartiallyResolved of
    Left interpretError -> throwError interpretError
    Right resolved -> pure resolved

  return $ last (Normalize.evaluate resolved)

-- | Errors related to interpretation of an expression
data InterpretError
  = ImportError Import.ImportError
  | ParseError Parser.ParseError
  deriving stock (Show)

instance Exception InterpretError where
  displayException (ImportError e) = displayException e
  displayException (ParseError e) = displayException e
