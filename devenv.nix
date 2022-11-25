{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.haskell-language-server
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
