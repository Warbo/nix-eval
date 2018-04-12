# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with {
  unstablePkgs = import ((import <nixpkgs> { config = {}; }).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "7b96c3a";
    sha256 = "0sqhf599ll2qshbnmspni5r3h5mfwbbkyz5xhfb3jxhflba594zw";
  }) {};
};

with builtins;
with unstablePkgs.nixpkgs1709.lib;
with rec {
  testScript = pkgs: pkgs.runCommand "nix-eval-test-script"
    {
      src = ./.;
    }
  ''
    mkdir "$out"
    echo "FIXME: Skipping test script" 1>&2
    #"$src/test.sh" 2> >(tee "$out/stderr" >&2) | tee "$out/stdout"
  '';

  withTests = pkgs: mapAttrs (_: pkgs.withDeps [ (testScript pkgs) ]);

  # "self" is a customised nixpkgs set, "super" is the corresponding original
  buildForNixpkgs = self: super: mapAttrs (_: buildForHaskell self)
                                          super.haskell.packages;

  buildForHaskell = pkgs: hsPkgs: withTests pkgs {
    # Uses Haskell package set provided by nixpkgs
    nixpkgsDeps  = hsPkgs.callPackage (pkgs.runCabal2nix { url = ./.; }) {};

    # Uses a Cabal sandbox to pick dependencies from (a snapshot of) Hackage
    hackageDeps = pkgs.haskellPkgWithDeps {
      inherit hsPkgs;
      delay-failure = true;
      dir           = ./.;
    };
  };
};

mapAttrs (name: self: buildForNixpkgs self (getAttr name unstablePkgs))
         unstablePkgs.customised
