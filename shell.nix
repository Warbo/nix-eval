{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, process, QuickCheck, stdenv, tasty
      , tasty-quickcheck
      }:
      mkDerivation {
        pname = "nix-eval";
        version = "0.1.0.2";
        src = ./.;
        libraryHaskellDepends = [ base process ];
        testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
        homepage = "http://chriswarbo.net/git/nix-eval";
        description = "Evaluate Haskell expressions using Nix to get packages";
        license = "GPL";
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
