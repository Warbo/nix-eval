# Used for building and testing on Hydra
with builtins;
with import ./nixpkgs.nix;
with lib;
with {
  nixpkgsVersion = fileContents (path + "/.version");
  ghcVersion     = haskellPackages.ghc.version;
};
{
  "nixpkgs${nixpkgsVersion}-ghc${ghcVersion}-nix-eval" =
    haskellPackages.nix-eval;
}
