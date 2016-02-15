module Paths_nix_eval where

getDataFileName :: FilePath -> IO FilePath
getDataFileName p = return ("./" ++ p)
