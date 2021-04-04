let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in
nixpkgs.haskellPackages.callCabal2nix "splitbot" ./. { }
