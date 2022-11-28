{ pkgs, ... }:

let
  haskell-app = import ./nix/haskell-app.nix { pkgs = pkgs; };
in
{
  # https://devenv.sh/basics/
  env = {
    inherit (haskell-app.env);
  };

  # https://devenv.sh/packages/
  packages = [
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.git
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.haskell-language-server
    pkgs.watchexec
    pkgs.zlib
  ];

  # enterShell = ''
  #   hello
  # '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;

  # https://devenv.sh/scripts/
  # scripts.hello.exec = "echo '🌀 Entering devenv shell'";

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";
}
