{ mkDerivation, base, process, QuickCheck, stdenv, tasty
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
}
