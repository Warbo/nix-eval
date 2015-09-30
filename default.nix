{ mkDerivation, base, process, QuickCheck, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "nix-eval";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base process ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  homepage = "http://chriswarbo.net/git/nix-eval";
  description = "Evaluate Haskell expressions using Nix to get packages";
  license = "GPL";
}
