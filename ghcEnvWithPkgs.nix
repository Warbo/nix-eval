with builtins;

{ hsPkgs ? null, pkgNames, name }:

let pkgs            = import <nixpkgs> {};
    envPkgs         = getEnv "NIX_EVAL_HASKELL_PKGS";
    haskellPackages = if hsPkgs == null
                         then if envPkgs == ""
                                 then pkgs.haskellPackages
                                 else import envPkgs
                         else hsPkgs;

 in pkgs.buildEnv {
      inherit name;
      paths = [ (haskellPackages.ghcWithPackages
                   (h: map (p: h."${p}") pkgNames)) ];
    }
