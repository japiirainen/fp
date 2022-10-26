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
import Fp.Input (Input (..))
import Options.Applicative (Alternative ((<|>)), Parser, ParserInfo)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Control.Monad.Except as Except
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Fp.Interpret as Interpret
import qualified Fp.Normalize as Normalize
import qualified Fp.Pretty
import qualified Fp.REPL as REPL
import qualified Fp.Width as Width
import qualified Options.Applicative as Options
import qualified Prettyprinter as Pretty
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.IO as IO

data Highlight
  = -- | Force the use of ANSI color escape sequences to highlight source code
    Color
  | -- | Don't highlight source code
    Plain
  | -- | Auto-detect whether to highlight source code based on whether or not
    --   @stdout@ is a terminal
    Auto

data Options
  = Interpret {file :: FilePath, highlight :: Highlight}
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

        highlight <- parseHighlight

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
  where
    parseHighlight =
      Options.flag'
        Color
        ( Options.long "color"
            <> Options.help "Enable syntax highlighting"
        )
        <|> Options.flag'
          Plain
          ( Options.long "plain"
              <> Options.help "Disable syntax highlighting"
          )
        <|> pure Auto

detectColor :: Highlight -> IO Bool
detectColor Color = do return True
detectColor Plain = do return False
detectColor Auto = do ANSI.hSupportsANSI IO.stdout

getRender :: Highlight -> IO (Doc AnsiStyle -> IO ())
getRender highlight = do
  color <- detectColor highlight
  width <- Width.getWidth

  return (Fp.Pretty.renderIO color width IO.stdout)

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

      render <- getRender highlight

      let syntax = Normalize.quote [] value

      render (Fp.Pretty.pretty syntax <> Pretty.hardline)
    REPL {} -> do
      REPL.repl
