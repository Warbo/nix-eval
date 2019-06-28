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
}
