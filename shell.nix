let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
pkgs.mkShell {
  buildInputs = [
    niv.niv
    watchexec

    cabal-install
    haskellPackages.ormolu
  ];

  inputsFrom = [ (haskellPackages.callCabal2nix "splitbot" ./. { }).env ];
}
