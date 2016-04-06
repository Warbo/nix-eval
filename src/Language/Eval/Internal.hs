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
import Paths_nix_eval (getDataFileName)
import System.Exit
import System.IO
import qualified System.IO.Strict
import System.IO.Unsafe
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
  cmd <- decideCmd x
  (out, code) <- runCmdStdIO cmd (buildInput f x)
  case code of
    ExitSuccess   -> return $ Just (trim out)
    ExitFailure _ -> hPutStr stderr out >> return Nothing

-- | Runs the given command, piping the given String into stdin, returning
--   stdout and the ExitCode. stderr is inherited.
runCmdStdIO :: CreateProcess -> String -> IO (String, ExitCode)
runCmdStdIO c i = do (Just hIn, Just hOut, Nothing, hProc) <- createProcess c
                     hPutContents hIn i
                     out  <- System.IO.Strict.hGetContents hOut
                     code <- waitForProcess hProc
                     return (out, code)

buildInput f x = unlines (map mkImport mods ++ ePreamble x ++ [f expr])
  where mods  = nub $ eMods x
        expr  = eExpr x

decideCmd :: Expr -> IO CreateProcess
decideCmd x = do
  newEnv <- needNewEnv (nub $ ePkgs x)
  return (buildCmd (if newEnv then mkCmd      x
                              else noShellCmd x))

buildCmd (cmd, args) = (proc cmd args) {
                         std_in  = CreatePipe,
                         std_out = CreatePipe,
                         std_err = Inherit
                       }

noShellCmd :: Expr -> (String, [String])
noShellCmd x = ("runhaskell", map (\(Flag x) -> x) (nub $ eFlags x))

hPutContents h c = hPutStr h c >> hClose h

-- | Construct the nix-shell command. We use wrapper.sh as a layer of
--   indirection, to work around buggy environments.
mkCmd :: Expr -> (String, [String])
mkCmd x = ("nix-shell", ["--show-trace", "--run", cmd, "-p", mkGhcPkg pkgs])
  where pkgs     = nub $ ePkgs x
        (rh, fs) = noShellCmd x
        run      = unwords (rh : fs)
        cmd      = wrapCmd run pkgs

wrapCmd c ps = "sh " ++ wrapperPath ++ " " ++ show c ++ " " ++ show (pkgsToName ps)

wrapperPath :: FilePath
{-# NOINLINE wrapperPath #-}
wrapperPath = unsafePerformIO (getDataFileName "wrapper.sh")

ghcEnvWithPkgsPath :: FilePath
{-# NOINLINE ghcEnvWithPkgsPath #-}
ghcEnvWithPkgsPath = unsafePerformIO (getDataFileName "ghcEnvWithPkgs.nix")

-- | Creates a Nix expression which will use ghcEnvWithPkgs.nix to make a
--   Haskell environment containing all of the given packages
mkGhcPkg ps = env ++ " { name = " ++ name ++ "; pkgNames = " ++ args ++ "; }"
  where env  = "import " ++ show ghcEnvWithPkgsPath
        args = "[ " ++ unwords pkgs ++ " ]"
        pkgs = map (\(Pkg p) -> show p) ps
        name = show (pkgsToName ps)

-- | This creates a name for our Haskell environment. We make it here once, and
--   pass it into both Nix and wrapper.sh, to ensure consistency
pkgsToName [] = "ghc-env"
pkgsToName ps = "ghc-env-with-" ++ intercalate "-" (map clean pkgs)
  where pkgs  = map (\(Pkg p) -> p) ps
        clean = filter isAlphaNum

-- | Check if all of the required packages are already available, i.e. whether
--   we need to create a new Haskell environment
havePkgs :: [Pkg] -> IO Bool
havePkgs []         = return True
havePkgs (Pkg p:ps) = do
  out <- readProcess "ghc-pkg" ["--simple-output", "list", p] ""
  if p `isInfixOf` out
     then havePkgs ps
     else return False

-- | Do we need to create a new Haskell environment, because we don't have
--   GHC available or because the packages we need aren't available?
needNewEnv ps = do
  hgp <- haveGhcPkg
  if hgp then fmap not (havePkgs ps)
         else return True

mkImport :: Mod -> String
mkImport (Mod m) = "import " ++ m

-- | Turn an expression into a Haskell module, complete with imports and `main`
mkHs :: String -> String
mkHs e = "main = putStr (" ++ e ++ ")"

-- | Strip leading and trailing whitespace
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Check if a shell command is available
haveCommand c = do
  (c, _, _) <- readCreateProcessWithExitCode (shell ("hash " ++ c)) ""
  return (c == ExitSuccess)

-- | Check if the `nix-shell` command is available via the shell
haveNix :: IO Bool
haveNix = haveCommand "nix-shell"

-- | Check if the `ghc-pkg` command is available via the shell
haveGhcPkg = haveCommand "ghc-pkg"

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
