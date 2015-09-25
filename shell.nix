{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, process, QuickCheck, stdenv, tasty
      , tasty-quickcheck
      }:
      mkDerivation {
        pname = "eval";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base process ];
        testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
        homepage = "http://chriswarbo.net/git/eval";
        description = "Evaluate Haskell expressions";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
