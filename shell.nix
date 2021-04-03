let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
  ghc = (import ./nix/ghc.nix).development;
in with nixpkgs;
pkgs.mkShell {
  buildInputs = [
    niv.niv
    watchexec

    cabal-install
    ghc
    haskellPackages.ormolu
  ];
}
