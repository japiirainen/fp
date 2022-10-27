{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Fp.Normalize (
  -- * Normalization
  evaluate,
  quote,

  -- * Errors related to normalization
  EvaluationError (..),
) where

import Control.Exception (Exception (displayException))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get, put))
import Data.List (transpose)
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)
import Fp.Input (Input)
import Fp.Location (Location)
import Fp.Pretty (Pretty)
import Fp.Syntax (Atom (..), Combinator1 (ApplyToAll, Insert), Combinator2 (Composition), Primitive (..), Syntax)
import Fp.Value (Value (..))
import Prelude hiding (succ)

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Fp.Location as Location
import qualified Fp.Pretty as Pretty
import qualified Fp.Syntax as Syntax
import qualified Fp.Value as Value
import qualified Fp.Width as Width

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
  -- | Evaluation environment
  Map Text Value ->
  -- | Surface syntax
  [Syntax Location Input] ->
  -- | result, free of reducible sub-expressions
  Either EvaluationError ([Value], Map Text Value)
evaluate bindings es = State.runStateT (mapM evalSingle es) bindings

evalSingle ::
  (MonadState (Map Text Value) m, MonadError EvaluationError m) =>
  Syntax Location Input ->
  m Value
evalSingle = \case
  Syntax.Variable {..} -> do
    env <- get
    case lookupVariable name env of
      Just value ->
        pure value
      Nothing -> throwError (UnboundVariable location name)
  Syntax.Definition {..} -> do
    v <- evalSingle body
    env <- get
    put $ Map.insert name v env
    evalSingle body
  Syntax.Application {..} -> do
    function' <- evalSingle function
    argument' <- evalSingle argument
    pure (apply function' argument')
  Syntax.Combinator1 {..} -> do
    argument' <- evalSingle argument
    pure $ Value.Combinator1 c1 argument'
  Syntax.Combinator2 {..} -> do
    argument1' <- evalSingle argument1
    argument2' <- evalSingle argument2
    pure $ Value.Combinator2 c2 argument1' argument2'
  Syntax.Atom {..} -> pure (Value.Atom atom)
  Syntax.Primitive {..} -> pure (Value.Primitive primitive)
  Syntax.List {..} -> do
    elements' <- mapM evalSingle elements
    if any (\case Syntax.Bottom {} -> True; _ -> False) elements
      then pure Value.Bottom
      else pure $ Value.List elements'
  Syntax.Construction {..} -> do
    functions' <- mapM evalSingle functions
    pure $ Value.Construction functions'
  Syntax.Bottom _ -> pure Value.Bottom

-- testExpr :: Input
-- testExpr = Code "test" "Def Plus = +\nDef Sum = /Plus\nSum:<1,2,3>"

-- >>> evaluate [] <$> Import.resolve testExpr
-- Not in scope: `Import.resolve'
-- No module named `Import' is imported.

{- | This is the function that implements function application,
     evaluating all built-in functions.
-}
apply :: Value -> Value -> Value
-- primitives
apply (Primitive AtomP) arg = case arg of
  Atom _ -> Atom (Bool True)
  Bottom -> Bottom
  _ -> Atom (Bool False)
apply (Primitive Eq) (List vs) = case vs of
  [v1, v2] -> Atom (Bool (v1 == v2))
  _ -> Bottom
apply (Primitive Null) arg = case arg of
  List xs -> if null xs then Atom (Bool True) else Atom (Bool False)
  Bottom -> Bottom
  _ -> Atom (Bool False)
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
apply (Primitive Reverse) (List vs) = List (reverse vs)
apply (Primitive Distl) (List vs) = case vs of
  [a, List xs] -> List $ map (\x -> List [a, x]) xs
  _ -> Bottom
apply (Primitive Distr) (List vs) = case vs of
  [List xs, a] -> List $ map (\x -> List [x, a]) xs
  _ -> Bottom
apply (Primitive Length) v = case v of
  List xs -> Atom (Int (length xs))
  _ -> Bottom
-- combinators
apply (Combinator1 Insert prim@(Primitive _)) (Value.List vs) =
  foldl1 (\acc x -> apply prim (List [acc, x])) vs
apply (Combinator1 ApplyToAll prim@(Primitive _)) (Value.List vs) =
  List $ map (apply prim) vs
apply (Combinator2 Composition f g) o =
  let o' = apply g o
   in apply f o'
-- construction
apply (Construction fns) arg = List (map (`apply` arg) fns)
apply v1 v2 = error $ show v1 <> " - " <> show v2 <> " not implemented!"

-- | Convert a `Value` back into the surface `Syntax`
quote ::
  -- | Variable names currently in scope (starting at @[]@ for a top-level
  --   expression)
  [Text] ->
  Value ->
  Syntax () Void
quote names value =
  case value of
    Value.Variable name ->
      Syntax.Variable {..}
    Value.Bottom ->
      Syntax.Bottom ()
    Value.Application function argument ->
      Syntax.Application
        { function = quote names function
        , argument = quote names argument
        , ..
        }
    Value.List elements ->
      Syntax.List {elements = fmap (quote names) elements, ..}
    Value.Construction functions ->
      Syntax.Construction {functions = fmap (quote names) functions, ..}
    Value.Atom atom ->
      Syntax.Atom {..}
    Value.Combinator1 c1 argument ->
      Syntax.Combinator1
        { argument = quote names argument
        , ..
        }
    Value.Combinator2 c2 argument1 argument2 ->
      Syntax.Combinator2
        { argument1 = quote names argument1
        , operatorLocation = ()
        , argument2 = quote names argument2
        , ..
        }
    Value.Primitive primitive ->
      Syntax.Primitive {..}
  where
    location = ()

data EvaluationError
  = UnboundVariable Location Text
  deriving (Eq, Show)

instance Exception EvaluationError where
  displayException (UnboundVariable location name) =
    "Unbound variable: "
      <> Text.unpack var
      <> "\n\
         \\n\
         \"
      <> Text.unpack (Location.renderError "" location)
    where
      var = prettyToText @(Syntax.Syntax () Void) Syntax.Variable {location = (), ..}

prettyToText :: Pretty a => a -> Text
prettyToText = Pretty.renderStrict False Width.defaultWidth
