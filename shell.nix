let
  sources = import ./nix/sources.nix;
  niv = import sources.niv { };
  nixpkgs = import sources.nixpkgs-unstable { };
  splitbot = import ./default.nix;
in with nixpkgs;
pkgs.mkShell {
  buildInputs = [
    niv.niv
    watchexec

    ghcid
    cabal-install
    haskellPackages.fourmolu
    haskellPackages.hasktags
  ];

  inputsFrom = [ splitbot.env ];
}
