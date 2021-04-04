{ ... }:

let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
pkgs.haskellPackages.callCabal2nix "splitbot" ./. { }
