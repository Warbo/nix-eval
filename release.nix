# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with builtins;
with rec {
  pkgSrc =
    with tryEval <nix-config>;
    if success
       then value
       else (import <nixpkgs> { config = {}; }).fetchgit {
              url    = http://chriswarbo.net/git/nix-config.git;
              rev    = "e409eae";
              sha256 = "1d4x14vv3xi53ib5ggyz2b4scj50smbkcnggzwy8z3zn3nym4pa4";
            };

  pkgs = import pkgSrc {};
};

with pkgs.lib;
{
  release = pkgs.haskellRelease {
    name        = "nix-eval";
    dir         = ./.;
    haskellKeep = hsVersion: !(hasPrefix "ghcjs"          hsVersion ||
                               hasPrefix "lts"            hsVersion ||
                               hasPrefix "ghc6"           hsVersion ||
                               hasPrefix "ghc72"          hsVersion ||
                               hasPrefix "ghcHaLVM"       hsVersion ||
                               hasPrefix "integer-simple" hsVersion ||
                               hasPrefix "ghcCross"       hsVersion ||
                               hasSuffix "HEAD"           hsVersion);
    nixKeep     = nixVersion: compareVersions nixVersion "nixpkgs1709" != -1;
  };
}
