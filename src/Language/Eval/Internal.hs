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

module Language.Eval.Internal where

import Data.Char
import Data.List
import Data.String
import System.Exit
import System.IO
import System.Process

-- We're stringly typed, with a veneer of safety

newtype Pkg  = Pkg String deriving (Eq, Ord, Show)
newtype Mod  = Mod String deriving (Eq, Ord, Show)
newtype Expr = Expr ([Pkg], [Mod], String) deriving (Show)

-- Allow permutations of package and module lists
instance Eq Expr where
  (Expr (p1, m1, e1)) == (Expr (p2, m2, e2)) = e1 == e2   &&
                                               perm p1 p2 &&
                                               perm m1 m2
    where perm  xs ys = allIn xs ys && allIn ys xs
          allIn xs ys = all (`elem` ys) xs

-- For convenience

instance IsString Expr where
  fromString = raw

instance IsString Pkg where
  fromString = Pkg

instance IsString Mod where
  fromString = Mod

-- Evaluation via `nix-shell`

-- | Evaluate an `Expr`; this is where the magic happens! If successful, returns
--   `Just` a `String`, which you can do what you like with.
eval :: Expr -> IO (Maybe String)
eval x@(Expr (pkgs, mods, expr)) = do
    (code, out, err) <- let (cmd, args) = mkCmd x
                         in readProcessWithExitCode cmd args (mkHs x)
    hPutStr stderr err
    return $ case code of
      ExitSuccess   -> Just (trim out)
      ExitFailure _ -> Nothing

mkCmd :: Expr -> (String, [String])
mkCmd (Expr (ps, _, _)) = ("nix-shell", ["--run", "runhaskell",
                                         "-p", mkGhcPkg ps])

-- The prefix "h." is arbitrary, as long as it matches the argument "h:"
mkGhcPkg ps = let pkgs = map (\(Pkg p) -> "(h." ++ p ++ ")") ps
               in concat ["haskellPackages.ghcWithPackages ",
                          "(h: [", unwords pkgs, "])"]

mkHs :: Expr -> String
mkHs (Expr (_, ms, e)) = unlines (imports ++ [main])
  where imports = map (\(Mod m) -> "import " ++ m) ms
        main    = "main = putStr (" ++ e ++ ")"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- User-facing combinators

-- | A raw String of Haskell code, with no packages or modules. You can use
--   OverloadedStrings to call this automatically.
raw :: String -> Expr
raw s = Expr ([], [], s)

-- | Apply the first Expr to the second, eg. `f $$ x` ==> `f x`
($$) :: Expr -> Expr -> Expr
(Expr (p1, m1, e1)) $$ (Expr (p2, m2, e2)) = Expr
  (nub (p1 ++ p2),
   nub (m1 ++ m2),
   concat ["((", e1, ") (", e2, "))"])

-- | Convert the argument to a String, then send to `raw`
asString :: (Show a) => a -> Expr
asString = raw . show

qualified :: Mod -> String -> Expr
qualified (Mod m) e = Expr ([], [Mod m], m ++ "." ++ e)

withMods :: [Mod] -> Expr -> Expr
withMods ms (Expr (ps, ms', e)) = Expr (ps, ms' ++ ms, e)

withPkgs :: [Pkg] -> Expr -> Expr
withPkgs ps (Expr (ps', ms, e)) = Expr (ps' ++ ps, ms, e)
