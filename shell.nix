let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
  splitbot = import ./default.nix;
in with nixpkgs;
pkgs.mkShell {
  buildInputs = [
    niv.niv
    watchexec

    cabal-install
    haskellPackages.ormolu
  ];

  inputsFrom = [ splitbot.env ];
}
