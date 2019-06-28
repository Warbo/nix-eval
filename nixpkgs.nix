# Pinned, known-good version of nixpkgs with our overlay
import (builtins.fetchTarball {
  name   = "nixpkgs1903";
  url    = https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz;
  sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
}) {
  config   = {};
  overlays = [ (import ./overlay.nix) ];
}
