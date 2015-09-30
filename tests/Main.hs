{-# LANGUAGE OverloadedStrings #-}

{-
nix-eval: Dependency-injecting Haskell evaluator

Copyright (C) 2015 Chris Warburton

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import Data.Maybe
import Language.Eval
import Language.Eval.Internal (haveNix)
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = do nix <- haveNix
          defaultMain $ testGroup "All tests" $ if nix then [
                testProperty "Can eval unit"       unitEval
              , testProperty "Can eval sums"       sumEval
              , testProperty "Can import modules"  modulesImport
              , testProperty "Can import packages" packagesImport
              ]
            else []

unitEval = checkIO "()" (Just "()")

sumEval :: [Int] -> Property
sumEval s = checkIO ("sum" $$ asString s)
                    (Just (show (sum s)))

modulesImport =
  checkIO (qualified "Data.Maybe" "isNothing" $$ "Nothing")
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
  result <- eval ("show" $$ i)
  return (result === o)
