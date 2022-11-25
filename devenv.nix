{ pkgs, ... }:

let
  splitbot = pkgs.haskellPackages.callPackage  (import ./default.nix) {};
in
{
  # https://devenv.sh/basics/
  env = {
    inherit (splitbot.env);
    GREET = "devenv";
  };

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.ghc
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.haskellPackages.fourmolu
    pkgs.zlib

    # temporary to test things out manually for development!
    pkgs.cabal2nix
  ];

  enterShell = ''
    hello
    git --version
  '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo hello from $GREET";


  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks.shellcheck.enable = true;

  pre-commit.hooks.cabal2nix.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";
}
