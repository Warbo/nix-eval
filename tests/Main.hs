module Main where

import Language.Eval.Internal
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can eval unit" unitEval
  ]

unitEval = monadicIO $ do
  result <- run $ eval (raw "()")
  assert (result == Just (raw "()"))
