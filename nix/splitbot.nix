{ tag }:

let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  ghc = (import ./ghc.nix).release;
in
derivation {
  name = "splitbot";
  builder = "${nixpkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  system = builtins.currentSystem;
  inherit tag;
  inherit ghc;

  src = builtins.fetchurl "https://github.com/juanedi/splitbot/archive/${tag}.tar.gz";

  coreutils = nixpkgs.coreutils;
  gnutar = nixpkgs.gnutar;
  gzip = nixpkgs.gzip;
}
