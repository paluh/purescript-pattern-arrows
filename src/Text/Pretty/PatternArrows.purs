module Text.Pretty.PatternArrows where

import Prelude
import Data.Tuple (Tuple(..), uncurry)
import Data.Array
import Data.Foldable
import Data.Maybe
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Trans

-- |
-- A first-order pattern match
--
-- A pattern is a Kleisli arrow for the @StateT Maybe@ monad. That is, patterns can fail, and can carry user-defined state.
--
data Pattern u a b = Pattern (a -> StateT u Maybe b)

runPattern :: forall u a b. Pattern u a b -> a -> StateT u Maybe b
runPattern (Pattern f) a = f a

instance functorPattern :: Functor (Pattern u a) where
  (<$>) f (Pattern p) = Pattern $ \a -> f <$> p a

instance categoryPattern :: Category (Pattern u) where
  id = Pattern $ \a -> return a
  (<<<) (Pattern f) (Pattern g) = Pattern $ f <=< g

instance arrowPattern :: Arrow (Pattern u) where
  arr f = Pattern $ \a -> return $ f a
  first p = Pattern $ \(Tuple b d) -> flip Tuple d <$> runPattern p b

instance arrowZeroPattern :: ArrowZero (Pattern u) where
  zeroArrow = Pattern $ \_ -> lift Nothing

instance arrowPlusPattern :: ArrowPlus (Pattern u) where
  (<+>) p q = Pattern $ \a -> StateT $ \s -> 
                case runStateT (runPattern p a) s of
                  Nothing -> runStateT (runPattern q a) s
                  Just r -> Just r

-- |
-- Run a pattern with an input and initial user state
--
-- Returns Nothing if the pattern fails to match
--
pattern :: forall u a b. Pattern u a b -> u -> a -> Maybe b
pattern p u = flip evalStateT u <<< runPattern p

-- |
-- Construct a pattern from a function
--
mkPattern :: forall u a b. (a -> Maybe b) -> Pattern u a b
mkPattern f = Pattern $ \a -> lift (f a)

-- |
-- Construct a pattern from a stateful function
--
mkPattern' :: forall u a b. (a -> StateT u Maybe b) -> Pattern u a b
mkPattern' = Pattern

fix :: forall u a r. (Pattern u a r -> Pattern u a r) -> Pattern u a r
fix f = Pattern $ \a -> runPattern (f $ fix f) a

-- |
-- Construct a pattern which recursively matches on the left-hand-side
--
chainl :: forall u a r. Pattern u a (Tuple a a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainl g f p = fix $ \c -> g >>> ((c <+> p) *** p) >>> arr (uncurry f)

-- |
-- Construct a pattern which recursively matches on the right-hand side
--
chainr :: forall u a r. Pattern u a (Tuple a a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainr g f p = fix $ \c -> g >>> (p *** (c <+> p)) >>> arr (uncurry f)

-- |
-- Construct a pattern which recursively matches on one-side of a tuple
--
wrap' :: forall u a s r. Pattern u a (Tuple s a) -> (s -> r -> r) -> Pattern u a r -> Pattern u a r
wrap' g f p = fix $ \c -> g >>> (id *** (c <+> p)) >>> arr (uncurry f)

-- |
-- Construct a pattern which matches a part of a tuple
--
split' :: forall u a s t r. Pattern u a (Tuple s t) -> (s -> t -> r) -> Pattern u a r
split' s f = s >>> arr (uncurry f)

-- |
-- An operator:
--
--  [@AssocL@] A left-associative operator
--
--  [@AssocR@] A right-associative operator
--
--  [@Wrap@] A prefix-like or postfix-like operator
--
--  [@Split@] A prefix-like or postfix-like operator which does not recurse into its operand
--
class Op op where
  assocL :: forall u a r.     Pattern u a (Tuple a a) -> (r -> r -> r) -> op u a r
  assocR :: forall u a r.     Pattern u a (Tuple a a) -> (r -> r -> r) -> op u a r
  wrap   :: forall u a s r.   Pattern u a (Tuple s a) -> (s -> r -> r) -> op u a r
  split  :: forall u a s t r. Pattern u a (Tuple s t) -> (s -> t -> r) -> op u a r

data Operator u a r = Operator (forall op. (Op op) => op u a r)

-- |
-- A table of operators
--
data OperatorTable u a r = OperatorTable [[Operator u a r]]

runOperatorTable :: forall u a r. OperatorTable u a r -> [[Operator u a r]]
runOperatorTable (OperatorTable table) = table

foldl1 :: forall a. (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

-- |
-- The target data type of the "Op" language, which modifies a Pattern
--
data PatternEndo u a r = PatternEndo (Pattern u a r -> Pattern u a r)

runPatternEndo :: forall u a r. PatternEndo u a r -> Pattern u a r -> Pattern u a r
runPatternEndo (PatternEndo f) p = f p

instance opPatternEndo :: Op PatternEndo where
  assocL pat g = PatternEndo $ chainl pat g
  assocR pat g = PatternEndo $ chainr pat g
  wrap   pat g = PatternEndo $ wrap'  pat g
  split  pat g = PatternEndo $ \_ -> split' pat g

-- |
-- Convert an operator to a PatternEndo
--
toPatternEndo :: forall u a r. Operator u a r -> Pattern u a r -> Pattern u a r
toPatternEndo (Operator op) = runPatternEndo op

-- |
-- Build a pretty printer from an operator table and an indecomposable pattern
--
buildPrettyPrinter :: forall u a r. OperatorTable u a r -> Pattern u a r -> Pattern u a r
buildPrettyPrinter table p = 
  let fs = map (map toPatternEndo) $ runOperatorTable table in
  foldl (\p' ops -> foldl1 (<+>) (flip map ops $ \f -> f p' <+> p')) p $ fs

