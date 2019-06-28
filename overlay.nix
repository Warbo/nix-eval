self: super:
with builtins;
with super.lib;
{
  haskell = super.haskell // {
    packages = mapAttrs
      (_: hsPkgs: hsPkgs.override (_: {
        overrides = helf: huper: {
          nix-eval = helf.callPackage
            (huper.haskellSrc2nix {
              name = "nix-eval";
              src  = filterSource
                (path: _: !(elem (baseNameOf path)) [
                  "README.md" "test.sh"

                  # VCS metadata
                  ".git" ".gitignore" ".issues"

                  # Left over build products
                  "dist" "dist-newstyle"

                  # Packaging metadata
                  "default.nix" "nixpkgs.nix" "overlay.nix" "release.nix"
                ])
                ./.;
            })
            {};
        };
      }))
      super.haskell.packages;
  };

  haskellPackages =
    with rec {
      stripNums     = replaceStrings (stringToCharacters "0123456789")
                                     ["" "" "" "" "" "" "" "" "" ""];

      normalVersion = name: stripNums name == "ghc";

      wanted      = hs: hs.ghc.version == super.haskellPackages.ghc.version;

      ghcMatches  = name: normalVersion name &&
                          wanted (getAttr name super.haskell.packages);

      ghcVersions = filter ghcMatches (attrNames self.haskell.packages);
    };
    if length ghcVersions == 1
       then getAttr (head ghcVersions) self.haskell.packages
       else abort (toJSON {
         error     = "Couldn't guess package set to use for haskellPackages";
         wantedGHC = super.haskellPackages.ghc.version;
         allSets   = attrNames self.haskell.packages;
         guessed   = ghcVersions;
       });
}
