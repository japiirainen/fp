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
import qualified Control.Monad.Except as Except
import qualified Fp.Import as Import
import qualified Fp.Lexer as Parser
import qualified Fp.Normalize as Normalize

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

  case Normalize.evaluate resolved of
    Left message -> Except.throwError (EvaluationError message)
    Right values -> pure (last values)

-- | Errors related to interpretation of an expression
data InterpretError
  = ImportError Import.ImportError
  | ParseError Parser.ParseError
  | EvaluationError Normalize.EvaluationError
  deriving stock (Show)

instance Exception InterpretError where
  displayException (ImportError e) = displayException e
  displayException (ParseError e) = displayException e
  displayException (EvaluationError e) = displayException e
