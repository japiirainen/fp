{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fp.Normalize where

import Control.Monad.State (MonadState (get, put), evalState, foldM, runState)
import Data.List (transpose)
import Data.Text (Text)
import Fp.Input (Input (Code))
import Fp.Location (Location)
import Fp.Syntax (Atom (..), Combinator1 (ApplyToAll, Insert), Combinator2 (Composition), Primitive (Divide, Minus, Plus, Times, Transpose), Syntax)
import Fp.Value (Value (..))
import Prelude hiding (succ)

import qualified Fp.Syntax as Syntax
import qualified Fp.Value as Value

{- | Lookup a variable from an ordered environment of name-value pairs using the
    variable's name
-}
lookupSymbol ::
  -- | Symbol name
  Text ->
  -- | Evaluation environment
  [(Text, Value)] ->
  Maybe Value
lookupSymbol name environment =
  case environment of
    (key, value) : rest ->
      if name == key
        then Just value
        else lookupSymbol name rest
    [] ->
      Nothing

evaluate ::
  [(Text, Value)] ->
  -- | Evaluation environment (starting at @[]@ for a top-level expression)
  [Syntax Location Input] ->
  -- | Surface syntax
  [Value] -- Result, free of reducible sub-expressions
evaluate environment ss = evalState (mapM go ss) environment
  where
    go :: MonadState [(Text, Value)] m => Syntax Location Input -> m Value
    go = \case
      Syntax.Definition {..} -> do
        v <- go body
        put $ (name, v) : environment
        go body
      Syntax.Application {..} -> do
        env <- get
        function' <- go function
        argument' <- go argument
        pure (evalState (apply function' argument') env)
      Syntax.Combinator1 {..} -> do
        argument' <- go argument
        pure $ Value.Combinator1 c1 argument'
      Syntax.Combinator2 {..} -> do
        argument1' <- go argument1
        argument2' <- go argument2
        pure $ Value.Combinator2 c2 argument1' argument2'
      Syntax.Atom {..} -> pure (Value.Atom atom)
      Syntax.Primitive {..} -> pure (Value.Primitive primitive)
      Syntax.List {..} -> do
        elements' <- mapM go elements
        pure $ Value.List elements'
      Syntax.Bottom _ -> pure Value.Bottom

testExpr :: Input
testExpr = Code "test" "Def IP ≡ (/+)∘(α*)∘Trans\nIP:<<1,2,3>,<6,5,4>>"

{- | This is the function that implements function application,
     evaluating all built-in functions.
-}
apply :: MonadState [(Text, Value)] m => Value -> Value -> m Value
apply (Primitive Plus) (List vs) = case take 2 vs of
  [Value.Atom (Int x), Atom (Int y)] -> pure (Atom (Int (x + y)))
  _ -> pure Bottom
apply (Primitive Times) (List vs) = case take 2 vs of
  [Atom (Int x), Atom (Int y)] -> pure (Atom (Int (x * y)))
  _ -> pure Bottom
apply (Primitive Minus) (List vs) = case take 2 vs of
  [Atom (Int x), Atom (Int y)] -> pure (Atom (Int (x - y)))
  _ -> pure Bottom
apply (Primitive Divide) (List vs) = case take 2 vs of
  [Atom (Int x), Atom (Int y)] ->
    pure (if y == 0 then Bottom else Atom (Int (x `div` y)))
  _ -> pure Bottom
apply (Primitive Transpose) (List vs) =
  let unwrap (List ys) = ys; unwrap _ = []
   in pure (List (List <$> transpose (unwrap <$> vs)))
apply (Combinator1 Insert (Primitive Plus)) (Value.List vs) =
  foldM
    (\acc x -> apply (Primitive Plus) (List [acc, x]))
    (Value.Atom (Int 0))
    vs
apply (Combinator1 ApplyToAll (Primitive Times)) (Value.List vs) = do
  vs' <- mapM (apply (Primitive Times)) vs
  pure (List vs')
apply (Combinator2 Composition f g) o = apply f =<< apply g o
apply (Atom (Symbol name)) o = do
  env <- get
  case lookupSymbol name env of
    Just f -> apply f o
    Nothing -> pure Bottom
apply v1 v2 = error $ show v1 <> " - " <> show v2 <> " not implemented!"
