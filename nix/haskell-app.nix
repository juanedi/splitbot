{ pkgs }: pkgs.haskellPackages.callCabal2nix "splitbot" ./.. { }
