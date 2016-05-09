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
              , testProperty "Nesting works"       checkNesting
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

-- Run nix-shells inside nix-shells, each with different Haskell packages
-- available, to make sure the environments are set up correctly
checkNesting = checkIO' id expr (Just "True")
  where expr   = unsafe $$ runHaskell nested
        -- Runs a String of Haskell in a sub-process
        runHaskell s = ((process $$ asString "runhaskell") $$ "[]") $$ asString s
        process    = withPkgs ["process"] $ qualified "System.Process" "readProcess"
        -- A String of Haskell code, which should invoke several `runhaskell`
        -- instances inside `nix-shell`s with different elements of `pkgs`
        -- available. The inner layer executes `base`, the others just propagate
        -- the result back up to us
        nested = recursiveHaskell pkgs base
        -- A selection of non-default packages/modules
        pkgs = [(Pkg "text",       Mod "Data.Text"),
                (Pkg "containers", Mod "Data.Graph"),
                (Pkg "parsec",     Mod "Text.Parsec")]
        -- This value should be propagated up through each putStrLn
        base = "main = putStr \"True\""

-- Helpers

-- | `checkIO' gen x s` runs `gen x` to get an `Expr`, evaluates it, and asserts
--   that the result equals `s`
checkIO' :: Show a => (a -> Expr) -> a -> Maybe String -> Property
checkIO' pre i o = once $ monadicIO $ do
  dbg (("expression", pre i),
       ("expected",   o))
  result <- run $ eval (pre i)
  dbg ("result", result)
  assert (result == o)

checkIO :: Expr -> Maybe String -> Property
checkIO = checkIO' ("show" $$)

mkHaskell :: Pkg -> Mod -> String -> String
mkHaskell p (Mod m) str = unlines [
          "import System.Process",
          "main = System.Process.readProcess",
          indent (show cmd),
          indent args',
          indent (show inner ++ " >>= putStrLn")]
  where inner = "import " ++ m ++ "\n" ++ str
        args' = "[" ++ intercalate "," (map show args) ++ "]"
        indent = ("  " ++)
        (cmd, args) = mkCmd expr
        expr = withPkgs [p] . withMods [Mod m] $ "undefined"

pkgString :: [Pkg] -> String
pkgString [Pkg p] = "haskellPackages.ghcWithPackages (h: [ h." ++ p ++ "])"

recursiveHaskell :: [(Pkg, Mod)] -> String -> String
recursiveHaskell []          base = base
recursiveHaskell ((p, m):xs) base = mkHaskell p m (recursiveHaskell xs base)

unsafe = qualified "System.IO.Unsafe" "unsafePerformIO"

dbg :: (Monad m, Show a) => a -> PropertyM m ()
dbg = monitor . counterexample . show
