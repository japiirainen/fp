{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the implementation of the @fp repl@ subcommand
module Fp.REPL (
  -- * REPL
  repl,
) where

import Control.Exception.Safe (displayException, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState (..))
import Fp.Interpret (Input (..))
import System.Console.Haskeline (Interrupt (..))
import System.Console.Repline (CompleterStyle (..), MultiLine (..), ReplOpts (..))
import Prelude hiding (lex)

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Fp.Interpret as Interpret
import qualified Fp.Lexer as Lexer
import qualified Fp.Normalize as Normalize
import qualified Fp.Parser as Parser
import qualified Fp.Pretty as Pretty
import qualified Fp.Value as Value
import qualified Fp.Width as Width
import qualified System.Console.Repline as Repline
import qualified System.IO as IO

-- | Entrypoint for the @fp repl@ subcommand
repl :: IO ()
repl = do
  let interpret input = do
        context <- get

        Except.runExceptT (Interpret.interpretWith context input)

  let err e =
        liftIO (Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e)))

  let lex input = do
        let eitherTokens = Lexer.lex "(input)" (Text.pack input)

        case eitherTokens of
          Left e -> do
            err e
          Right tokens -> do
            liftIO $ putStrLn "------RAW_TOKENS-----"
            liftIO (print tokens)
            liftIO $ putStrLn "------END_TOKENS-----"

  let parse input = do
        let eitherSyntax = Parser.parse "(input)" (Text.pack input)

        case eitherSyntax of
          Left e -> do
            err e
          Right syntax -> do
            width <- liftIO Width.getWidth
            liftIO $ putStrLn "------RAW_SYNTAX-----"
            liftIO (print syntax)
            liftIO $ putStrLn "----PRETTY_SYNTAX----"
            Monad.forM_ syntax \s -> do
              let ss = bimap (const ()) (const ()) s
              liftIO $
                Pretty.renderIO True width IO.stdout (Pretty.pretty ss <> "\n")
            liftIO $ putStrLn "------END_SYNTAX-----"

  let command string = do
        let input = Code "(input)" (Text.pack string)

        eitherResult <- interpret input

        case eitherResult of
          Left e -> do
            err e
          Right (values, _) -> do
            Monad.forM_ values \value -> do
              Monad.when (Value.shouldShow value) do
                let syntax = Normalize.quote [] value
                width <- liftIO Width.getWidth
                liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty syntax <> "\n"))

  let help _string = do
        liftIO
          ( putStrLn
              "Type any expression to normalize it or use one of the following commands:\n\
              \:help\n\
              \    Print help text and describe options\n\
              \:paste\n\
              \    Start a multi-line input. Submit with <Ctrl-D>\n\
              \:let IDENTIFIER = EXPRESSION\n\
              \    Assign an expression to a variable\n\
              \:lex EXPRESSION\n\
              \    Run the expression throught Lexer.lex\n\
              \:parse EXPRESSION\n\
              \    Run the expression throught Parser.parse\n\
              \:quit\n\
              \    Exit the REPL"
          )

  let assign var expr = do
        let variable = Text.strip (Text.pack var)

        let input =
              Code
                "(input)"
                ("Def " <> variable <> " = " <> Text.pack expr)

        eitherResult <- interpret input

        case eitherResult of
          Left e -> do
            err e
          Right (_, bindings) -> do
            State.modify (bindings <>)

  let assignment string
        | (var, '=' : expr) <- break (== '=') string = assign var expr
        | (var, '≡' : expr) <- break (== '≡') string = assign var expr
        | otherwise = liftIO (putStrLn "usage: let = {expression}")

  let quit _ =
        liftIO (throwIO Interrupt)

  let options =
        [ ("help", Repline.dontCrash . help)
        , ("let", Repline.dontCrash . assignment)
        , ("lex", Repline.dontCrash . lex)
        , ("parse", Repline.dontCrash . parse)
        , -- `paste` is included here for auto-completion purposes only.
          -- `repline`'s `multilineCommand` logic overrides this no-op.
          ("paste", Repline.dontCrash . \_ -> return ())
        , ("quit", quit)
        ]

  let banner MultiLine = return "... "
      banner SingleLine = return "λ "

  let prefix = Just ':'

  let multilineCommand = Just "paste"

  let initialiser = liftIO (putStrLn "\nWelcome to the fp REPL!")

  let finaliser = do
        liftIO (putStrLn "Until next time!")
        return Repline.Exit

  let tabComplete = Word0 (\_ -> pure [])

  let action = Repline.evalReplOpts ReplOpts {..}

  State.evalStateT action mempty
