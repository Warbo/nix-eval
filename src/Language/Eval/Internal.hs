module Language.Eval.Internal where

import System.Exit
import System.IO
import System.Process

newtype Pkg  = Pkg String deriving (Eq, Ord, Show)
newtype Mod  = Mod String deriving (Eq, Ord, Show)
newtype Expr = Expr ([Pkg], [Mod], String)

instance Eq Expr where
  (Expr (p1, m1, e1)) == (Expr (p2, m2, e2)) = e1 == e2   &&
                                               perm p1 p2 &&
                                               perm m1 m2
    where perm  xs ys = allIn xs ys && allIn ys xs
          allIn xs ys = all (`elem` ys) xs

raw :: String -> Expr
raw s = Expr ([], [], s)

eval :: Expr -> IO (Maybe Expr)
eval x@(Expr (pkgs, mods, expr)) = do
    (code, out, err) <- let (cmd, args) = mkCmd x
                         in readProcessWithExitCode cmd args (mkHs x)
    hPutStr stderr err
    return $ case code of
      ExitSuccess   -> Just (Expr (pkgs, mods, trim out))
      ExitFailure _ -> Nothing

mkCmd :: Expr -> (String, [String])
mkCmd x = ("", [])

mkHs :: Expr -> String
mkHs = const ""

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
