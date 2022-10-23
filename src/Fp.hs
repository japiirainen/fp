{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | This module contains the top-level `main` function that implements the
    command-line API
-}
module Fp (
  -- * Main
  main,
) where

import Control.Exception.Safe (Exception (..))
import Fp.Interpret (Input (..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Monad.Except as Except
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Fp.Interpret as Interpret
import qualified Fp.REPL as REPL
import qualified Options.Applicative as Options
import qualified System.Exit as Exit
import qualified System.IO as IO

data Options
  = Interpret {file :: FilePath}
  | REPL {}

parserInfo :: ParserInfo Options
parserInfo =
  Options.info
    (Options.helper <*> parser)
    (Options.progDesc "Command-line utility for the `fp` programming language")

parser :: Parser Options
parser = do
  let interpret = do
        file <-
          Options.strArgument
            ( Options.help "File to interpret"
                <> Options.metavar "FILE"
            )

        return Interpret {..}

  let repl = do
        pure REPL {}

  Options.hsubparser
    ( Options.command
        "interpret"
        ( Options.info
            interpret
            (Options.progDesc "Interpret a `fp` file")
        )
        <> Options.command
          "repl"
          ( Options.info
              repl
              (Options.progDesc "Enter a REPL for `fp`")
          )
    )

throws :: Exception e => Either e a -> IO a
throws (Left e) = do
  Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e))
  Exit.exitFailure
throws (Right result) = do
  return result

-- | Command-line entrypoint
main :: IO ()
main = do
  options <- Options.execParser parserInfo

  case options of
    Interpret {..} -> do
      input <- case file of
        "-" -> do
          Code "(input)" <$> Text.IO.getContents
        _ -> do
          return (Path file)

      eitherResult <- do
        Except.runExceptT (Interpret.interpret input)
      value <- throws eitherResult
      print value
    REPL {} -> do
      REPL.repl