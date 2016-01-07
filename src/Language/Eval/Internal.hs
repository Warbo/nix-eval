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

newtype Pkg  = Pkg  String deriving (Eq, Ord, Show)
newtype Mod  = Mod  String deriving (Eq, Ord, Show)
newtype Flag = Flag String deriving (Eq, Ord, Show)

data Expr = Expr {
    ePkgs :: [Pkg]
    -- ^ Packages
  , eMods :: [Mod]
    -- ^ Modules
  , eFlags :: [Flag]
    -- ^ Required runhaskell arguments
  , ePreamble :: [String]
    -- ^ Extra lines to add to the generated module's top-level
  , eExpr :: String
    -- ^ The Haskell expression
  } deriving (Show)

-- Allow permutations of package and module lists
instance Eq Expr where
  e1 == e2 = eExpr e1 == eExpr e2       &&
             perm (ePkgs e1) (ePkgs e2) &&
             perm (eMods e1) (eMods e2) &&
             perm (eFlags e1) (eFlags e2)
    where perm  xs ys = allIn xs ys && allIn ys xs
          allIn xs ys = all (`elem` ys) xs

-- For convenience

instance IsString Expr where
  fromString = raw

instance IsString Pkg where
  fromString = Pkg

instance IsString Mod where
  fromString = Mod

instance IsString Flag where
  fromString = Flag

-- Evaluation via `nix-shell`

-- | Evaluate an `Expr`; this is where the magic happens! If successful, returns
--   `Just` a `String`, which you can do what you like with.
eval :: Expr -> IO (Maybe String)
eval = eval' mkHs

-- | Same as `eval`, but allows a custom formatting function to be supplied, eg.
--   if you want an alternative to the default "main = putStr (..)" behaviour.
eval' :: (String -> String) -> Expr -> IO (Maybe String)
eval' f x = do
  let mods  = eMods x
      expr  = eExpr x
  (code, out, err) <- let (cmd, args) = mkCmd x
                       in readProcessWithExitCode cmd
                                                  args
                                                  (unlines (pragma (eFlags x) ++
                                                            map mkImport mods ++
                                                            ePreamble x       ++
                                                            [f expr]))
  hPutStr stderr err
  return $ case code of
    ExitSuccess   -> Just (trim out)
    ExitFailure _ -> Nothing

pragma [] = []
pragma xs = let fs = map (\(Flag x) -> x) xs
             in [concat ["{-# LANGAUGE ", intercalate ", " fs, "#-}"]]

mkCmd :: Expr -> (String, [String])
mkCmd x = ("nix-shell", ["--run", run, "-p", mkGhcPkg pkgs])
  where pkgs = ePkgs x
        run  = unwords ("runhaskell" : map (\(Flag x) -> x) (eFlags x))

-- The prefix "h." is arbitrary, as long as it matches the argument "h:"
mkGhcPkg ps = let pkgs = map (\(Pkg p) -> "(h." ++ p ++ ")") ps
               in concat ["haskellPackages.ghcWithPackages ",
                          "(h: [", unwords pkgs, "])"]

mkImport :: Mod -> String
mkImport (Mod m) = "import " ++ m

-- | Turn an expression into a Haskell module, complete with imports and `main`
mkHs :: String -> String
mkHs e = "main = putStr (" ++ e ++ ")"

-- | Strip leading and trailing whitespace
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Check if the `nix-shell` command is available via the shell
haveNix :: IO Bool
haveNix = do
  (c, _, _) <- readCreateProcessWithExitCode (shell "hash nix-shell") ""
  return (c == ExitSuccess)

-- User-facing combinators

-- | A raw String of Haskell code, with no packages or modules. You can use
--   OverloadedStrings to call this automatically.
raw :: String -> Expr
raw s = Expr { ePkgs = [], eMods = [], eExpr = s, eFlags = [], ePreamble = [] }

-- | Apply the first Expr to the second, eg. `f $$ x` ==> `f x`
infixr 8 $$
($$) :: Expr -> Expr -> Expr
x $$ y = Expr {
  ePkgs     = nub (ePkgs     x ++ ePkgs     y),
  eMods     = nub (eMods     x ++ eMods     y),
  eFlags    = nub (eFlags    x ++ eFlags    y),
  ePreamble =      ePreamble x ++ ePreamble y ,
  eExpr  = concat ["((", eExpr x, ") (", eExpr y, "))"] }

-- | Convert the argument to a String, then send to `raw`
asString :: (Show a) => a -> Expr
asString = raw . show

-- | Qualify an expression, eg. `qualified "Data.Bool" "not"` gives the
--   expression `Data.Bool.not` with "Data.Bool" in its module list
qualified :: Mod -> Expr -> Expr
qualified (Mod m) x = x { eMods = Mod m : eMods x,
                          eExpr = m ++ "." ++ eExpr x }

-- | Append modules to an expression's context
withMods :: [Mod] -> Expr -> Expr
withMods ms x = x { eMods = eMods x ++ ms }

-- | Append packages to an expression's context
withPkgs :: [Pkg] -> Expr -> Expr
withPkgs ps x = x { ePkgs = ePkgs x ++ ps }

-- | Append arguments to an expression's context
withFlags :: [Flag] -> Expr -> Expr
withFlags fs x = x { eFlags = eFlags x ++ fs }

withPreamble :: String -> Expr -> Expr
withPreamble p x = x { ePreamble = ePreamble x ++ [p] }
