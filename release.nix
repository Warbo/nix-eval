# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with builtins;
with {
  helpersSrc = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "d012fd6";
    sha256 = "1lbjsn2z6d80df8qw7izsiszpdvj6bmpy179pqar9gdfgwlbl9rv";
  };
};

with import helpersSrc;
collapseAttrs (haskellRelease {
  name        = "nix-eval";
  dir         = ./.;
  hackageSets = { nixpkgs1709 = [ "ghc7103" ]; };
})
