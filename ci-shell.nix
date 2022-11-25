let
  pkgs = import ./pkgs.nix { };
  devenv = import ./devenv.nix { pkgs = pkgs; };
in
with pkgs;
mkShell {
  buildInputs = devenv.packages;
}
