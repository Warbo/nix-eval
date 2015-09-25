{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Eval.Internal
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can eval unit"       unitEval
  , testProperty "Can eval sums"       sumEval
  , testProperty "Can import modules"  modulesImport
  , testProperty "Can import packages" packagesImport
  ]

unitEval = checkIO "()" (Just "()")

sumEval :: [Int] -> Property
sumEval s = checkIO ("sum" $$ asString s)
                    (Just (show (sum s)))

modulesImport = checkIO (qualified "Data.Maybe" "isNothing" $$ "Nothing")
                        (Just "True")

packagesImport :: String -> Property
packagesImport s = let expr = len $$ (pack $$ asString s)
                       len  = qualified "Data.Text" "length"
                       pack = qualified "Data.Text" "pack"
  in checkIO (withPkgs ["text"] expr)
             (Just (show (length s)))


-- Helpers

checkIO :: Expr -> Maybe String -> Property
checkIO i o = once $ ioProperty $ do
  result <- eval i
  return (result === o)
