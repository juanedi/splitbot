{ system ? builtins.currentSystem, imageName, tag }:

let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  splitbot = import ./splitbot.nix { inherit splitbot; };

in pkgs.dockerTools.buildLayeredImage {
  name = imageName;
  inherit tag;
  contents = [ splitbot ];

  config = {
    Cmd = [ "/bin/splitbot" "--server" ];
    WorkingDir = "/";
  };
}
