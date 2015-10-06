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

module Language.Eval (
    Pkg(..)
  , Mod(..)
  , Flag(..)
  , Expr(..)
  , eval
  , raw
  , ($$)
  , asString
  , qualified
  , withMods
  , withPkgs
  , withFlags) where

import Language.Eval.Internal
