module Main where

import Prelude
import Data.Maybe
import Data.Tuple
import Control.Arrow ((<+>))
import Text.Pretty.PatternArrows

data Expr 
  = Lam String Expr
  | App Expr Expr
  | Var String

parenthesize :: forall u a. Pattern u a String -> Pattern u a String
parenthesize = (<$>) (\s -> "(" ++ s ++ ")")

var :: forall u. Pattern u Expr String
var = mkPattern var'

var' :: Expr -> Maybe String
var' (Var s) = Just s
var' _ = Nothing

lam :: forall u. Pattern u Expr (Tuple String Expr)
lam = mkPattern lam'

lam' :: Expr -> Maybe (Tuple String Expr)
lam' (Lam s e) = Just (Tuple s e)
lam' _ = Nothing

app :: forall u. Pattern u Expr (Tuple Expr Expr)
app = mkPattern app'

app' :: Expr -> Maybe (Tuple Expr Expr)
app' (App e1 e2) = Just (Tuple e1 e2)
app' _ = Nothing

expr :: forall u. Pattern u Expr String
expr = fix $ \p -> buildPrettyPrinter opTable (var <+> parenthesize p)

opTable :: forall u. OperatorTable u Expr String
opTable = OperatorTable
          [ [ Operator (assocL app $ \e1 e2 -> e1 ++ " " ++ e2) ]
          , [ Operator (wrap lam $ \b s -> "\\" ++ b ++ " -> " ++ s) ]
          ]

printExpr :: Expr -> String
printExpr = fromMaybe ("Incomplete pattern match") <<< pattern expr {}

testExpr :: Expr
testExpr = Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

main = Debug.Trace.trace $ printExpr testExpr
