{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the import resolution logic
module Fp.Import (
  -- * Import resolution
  resolve,

  -- * Exceptions
  ImportError (..),
) where

import Control.Exception.Safe (Exception (..))
import Data.Bifunctor (first)
import Data.Text (Text)
import Fp.Input (Input (..))
import Fp.Location (Location (..))
import Fp.Syntax (Syntax)

import qualified Control.Exception.Safe as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Fp.Parser as Parser
import qualified Text.URI as URI

-- | Resolve an `Input` by returning the source code that it represents
resolve :: Input -> IO ([Syntax Location Input])
resolve input = case input of
  URI _ -> throw "URI import not implemented!"
  Path path -> readPath path
  Code name code -> do
    result <- case Parser.parse name code of
      Left e -> Exception.throw e
      Right result -> return result

    let locate offset = Location {..}

    return $ map (first locate) result
  where
    readPath path = do
      code <- Text.IO.readFile path
      result <- case Parser.parse path code of
        Left e -> Exception.throw e
        Right result -> return result
      let locate offset = Location {name = path, ..}
      return $ map (first locate) result

    throw e = Exception.throw (ImportError input e)

-- | The base error for `ImportError` (without the @input@ information)

-- | Errors related to import resolution
data ImportError = ImportError
  { input :: Input
  , message :: Text
  }
  deriving stock (Show)

instance Exception ImportError where
  displayException ImportError {..} =
    Text.unpack
      ( "Import resolution failed: " <> renderedInput
          <> "\n\
             \\n\
             \"
          <> message
      )
    where
      renderedInput = case input of
        URI uri -> URI.render uri
        Path path -> Text.pack path
        Code _ _ -> "(input)"
