let pkgs = import <nixpkgs> {}; in
{ hsPkgs ? pkgs.haskellPackages, pkgNames, name }:

pkgs.buildEnv {
  inherit name;
  paths = [ (hsPkgs.ghcWithPackages (h: map (p: h.${p}) pkgNames)) ];
}
