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

import Data.List
import Data.Maybe
import Language.Eval
import Language.Eval.Internal
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

-- | Only test if `nix-shell` is available. In particular, when Nix builds and
--   tests this package, it uses a pure environment where nix-shell isn't
--   available
main = do nix <- haveNix
          defaultMain $ testGroup "All tests" $ if nix then [
                testProperty "Can eval unit"       unitEval
              , testProperty "Can eval sums"       sumEval
              , testProperty "Can import modules"  modulesImport
              , testProperty "Can import packages" packagesImport
              , testProperty "$$ precedence"       precedence
              , testProperty "Preamble added"      preambleAdded
              , testProperty "Flags work"          checkFlags
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

precedence :: String -> Property
precedence s =
  checkIO (withPkgs ["text"] (qualified "Data.Text" "unpack" $$
                              qualified "Data.Text" "pack"   $$
                              asString s))
          (Just (show s))

preambleAdded :: Int -> Property
preambleAdded i =
  checkIO (withPreamble ("foo = " ++ show i) "foo * foo")
          (Just (show (i * i)))

checkFlags :: Int -> Property
checkFlags i = checkIO e (Just (show i))
  where e = withFlags ["-XTemplateHaskell"] . raw $
                "$([|" ++ show i ++ "|])"

-- Helpers

checkIO :: Expr -> Maybe String -> Property
checkIO i o = once $ ioProperty $ do
  result <- eval ("show" $$ i)
  return (result === o)
