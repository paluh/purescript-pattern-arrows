module Text.Pretty.PatternArrows where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.State (StateT(..), evalStateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Plus (class Plus)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, foldl1)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong, (***))
import Data.Tuple (Tuple, uncurry)

-- |
-- A first-order pattern match
--
-- A pattern is a Kleisli arrow for the @StateT Maybe@ monad. That is, patterns can fail, and can carry user-defined state.
--
-- | `Star` has no `Lazy` instance
newtype Pattern u a b = Pattern (Star (StateT u Maybe) a b)
derive newtype instance semigroupoidPattern :: Semigroupoid (Pattern u)
derive newtype instance categoryPattern :: Category (Pattern u)
derive newtype instance profunctorPattern :: Profunctor (Pattern u)
derive newtype instance strongPattern :: Strong (Pattern u)
derive newtype instance functorPattern :: Functor (Pattern u a)
derive newtype instance applyPattern :: Apply (Pattern u a)
derive newtype instance applicativePattern :: Applicative (Pattern u a)
instance altPattern :: Alt (Pattern u a) where
  alt f g = Pattern $ Star $ \a -> StateT \u ->
    case runStateT (runPattern f a) u of
      Nothing -> runStateT (runPattern g a) u
      r -> r
derive newtype instance plusPattern :: Plus (Pattern u a)
derive newtype instance alternativePattern :: Alternative (Pattern u a)
instance lazyPattern :: Lazy (Pattern u a b) where
    defer f = Pattern (Star \x -> runPattern (f unit) x)

runPattern :: forall a b u. Pattern u a b -> (a -> StateT u Maybe b)
runPattern (Pattern (Star f)) = f

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
mkPattern f = Pattern $ Star $ \a -> lift (f a)

-- |
-- Construct a pattern from a stateful function
--
mkPattern' :: forall u a b. (a -> StateT u Maybe b) -> Pattern u a b
mkPattern' = Pattern <<< Star

fix :: forall u a r. (Pattern u a r -> Pattern u a r) -> Pattern u a r
fix f = Pattern $ Star $ \a -> runPattern (f $ fix f) a

-- |
-- Construct a pattern which recursively matches on the left-hand-side
--
chainl :: forall u a r. Pattern u a (Tuple a a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainl split f p = fix $ \c -> (split >>> ((c <|> p) *** p) >>> mkPattern' (uncurry f >>> pure))

-- |
-- Construct a pattern which recursively matches on the right-hand side
--
chainr :: forall u a r. Pattern u a (Tuple a a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainr g f p = fix $ \c -> g >>> (p *** (c <|> p)) >>> mkPattern' (uncurry f >>> pure)

-- |
-- Construct a pattern which recursively matches on one-side of a tuple
--
wrap' :: forall u a s r. Pattern u a (Tuple s a) -> (s -> r -> r) -> Pattern u a r -> Pattern u a r
wrap' g f p = fix $ \c -> g >>> (identity *** (c <|> p)) >>> mkPattern' (uncurry f >>> pure)

-- |
-- Construct a pattern which matches a part of a tuple
--
split' :: forall u a s t r. Pattern u a (Tuple s t) -> (s -> t -> r) -> Pattern u a r
split' s f = s >>> mkPattern' (uncurry f >>> pure)

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
data OperatorTable u a r = OperatorTable (Array (NonEmpty Array (Operator u a r)))

runOperatorTable :: forall u a r. OperatorTable u a r -> Array (NonEmpty Array (Operator u a r))
runOperatorTable (OperatorTable table) = table

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
  foldl (\p' ops -> foldl1 (<|>) (flip map ops $ \f -> f p' <|> p')) p $ fs

