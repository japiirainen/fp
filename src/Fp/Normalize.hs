{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fp.Normalize where

import Control.Monad.State (MonadState (get, put), evalState)
import Data.List (foldl', transpose)
import Data.Map (Map)
import Data.Text (Text)
import Fp.Input (Input (Code))
import Fp.Location (Location)
import Fp.Syntax (Atom (..), Combinator1 (ApplyToAll, Insert), Combinator2 (Composition), Primitive (Divide, Minus, Plus, Times, Transpose), Syntax)
import Fp.Value (Value (..))
import Prelude hiding (succ)

import Control.Exception (Exception (displayException))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Fp.Syntax as Syntax
import qualified Fp.Value as Value

{- | Lookup a variable from an ordered environment of name-value pairs using the
    variable's name
-}
lookupVariable ::
  -- | Symbol name
  Text ->
  -- | Evaluation environment
  Map Text Value ->
  Maybe Value
lookupVariable = Map.lookup

evaluate ::
  [Syntax Location Input] ->
  -- | Surface syntax
  [Value] -- Result, free of reducible sub-expressions
evaluate ss = evalState (mapM go ss) mempty
  where
    go ::
      (MonadState (Map Text Value) m) => Syntax Location Input -> m Value
    go = \case
      Syntax.Variable {..} -> do
        env <- get
        case lookupVariable name env of
          Just value ->
            pure value
          Nothing -> error "TODO: handle evaluation errors"
      Syntax.Definition {..} -> do
        v <- go body
        env <- get
        put $ Map.insert name v env
        go body
      Syntax.Application {..} -> do
        function' <- go function
        argument' <- go argument
        pure (apply function' argument')
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
testExpr = Code "test" "Def Plus = +\nDef Sum = /Plus\nSum:<1,2,3>"

-- >>> evaluate [] <$> Import.resolve testExpr
-- Combinator1 Insert (Atom (Symbol "Plus")) - List [Atom (Int 1),Atom (Int 2),Atom (Int 3)] not implemented!

{- | This is the function that implements function application,
     evaluating all built-in functions.
-}
apply :: Value -> Value -> Value
apply (Primitive Plus) (List vs) = case take 2 vs of
  [Value.Atom (Int x), Atom (Int y)] -> Atom (Int (x + y))
  _ -> Bottom
apply (Primitive Times) (List vs) = case take 2 vs of
  [Atom (Int x), Atom (Int y)] -> Atom (Int (x * y))
  _ -> Bottom
apply (Primitive Minus) (List vs) = case take 2 vs of
  [Atom (Int x), Atom (Int y)] -> Atom (Int (x - y))
  _ -> Bottom
apply (Primitive Divide) (List vs) = case take 2 vs of
  [Atom (Int x), Atom (Int y)] ->
    (if y == 0 then Bottom else Atom (Int (x `div` y)))
  _ -> Bottom
apply (Primitive Transpose) (List vs) =
  let unwrap (List ys) = ys; unwrap _ = []
   in List (List <$> transpose (unwrap <$> vs))
apply (Combinator1 Insert (Primitive Plus)) (Value.List vs) =
  foldl'
    (\acc x -> apply (Primitive Plus) (List [acc, x]))
    (Value.Atom (Int 0))
    vs
apply (Combinator1 ApplyToAll (Primitive Times)) (Value.List vs) =
  List $ map (apply (Primitive Times)) vs
apply (Combinator2 Composition f g) o =
  let o' = apply g o
   in apply f o'
apply v1 v2 = error $ show v1 <> " - " <> show v2 <> " not implemented!"

newtype EvaluationError
  = UnboundVariable Text
  deriving (Eq, Show)

instance Exception EvaluationError where
  displayException (UnboundVariable name) =
    Text.unpack ("Unbound variable: " <> name)
