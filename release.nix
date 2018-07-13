# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with builtins;
with {
  helpersSrc = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "42156fb";
    sha256 = "0pc88y1gbli2f8f54yg070ghpm4r1j3mkj6xkmphj189s9lsv1f7";
  };
};

with import helpersSrc;
collapseAttrs (haskellRelease {
  name            = "nix-eval";
  dir             = ./.;
  haskellVersions = [ "ghc7103" ];
  nixpkgsVersions = [ "nixpkgs1709" "nixpkgs1803" ];
})
