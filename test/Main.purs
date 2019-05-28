module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton) as NonEmpty
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal) as Assert
import Test.Unit.Main (runTest)
import Text.Pretty.PatternArrows (Operator(..), OperatorTable(..), Pattern, assocL, buildPrettyPrinter, mkPattern, pattern, wrap)

data LambdaExpr
  = Var String
  | Abs String LambdaExpr
  | App LambdaExpr LambdaExpr

abstraction :: forall u. Pattern u LambdaExpr (Tuple String LambdaExpr)
abstraction = mkPattern case _ of
  Abs x y -> Just (Tuple x y)
  otherwise -> Nothing

application :: forall u. Pattern u LambdaExpr (Tuple LambdaExpr LambdaExpr)
application = mkPattern $ case _ of
  App x y -> Just (Tuple x y)
  otherwise -> Nothing

variable :: forall u. Pattern u LambdaExpr String
variable = mkPattern $ case _ of
  Var x ->  Just x
  otherwise -> Nothing

table :: forall u. OperatorTable u LambdaExpr String
table = OperatorTable
  [ NonEmpty.singleton (Operator (assocL application $ \e1 e2 -> e1 <> " " <> e2))
  , NonEmpty.singleton (Operator (wrap abstraction $ \a e -> "\\" <> a <> " -> " <> e))
  ]

printer :: forall u. Pattern u LambdaExpr String
printer = fix \p ->
  buildPrettyPrinter table (variable <|> (\s -> "(" <> s <> ")") <$> p)

main :: Effect Unit
main = runTest do
  suite "Example LambdaExpr" do
    test "simple Var" do
      Assert.equal (pattern printer unit (Var "x")) (Just "x")

    test "abstractions application" do
      let
        expr = App (Abs "x" $ Var "x") (Abs "x" $ Var "x")
      Assert.equal (pattern printer unit expr) (Just "(\\x -> x) (\\x -> x)")

    test "application chain" do
      let
        expr =
          Abs "x" $
            Abs "y" $
              Abs "z" $
                App
                  (App (Var "x") (Var "z"))
                  (App (Var "y") (Var "z"))

      Assert.equal (pattern printer unit expr) (Just  "\\x -> \\y -> \\z -> x z (y z)")

