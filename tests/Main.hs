module Main where

import Language.Eval.Internal
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can eval unit" unitEval
  ]

unitEval = once $ ioProperty $ do
  result <- eval (raw "()")
  return (result === Just (raw "()"))

--debug = monitor . counterexample . show
