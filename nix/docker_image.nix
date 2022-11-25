{ system ? builtins.currentSystem, imageName, tag }:

let
  pkgs = import ../pkgs.nix { };
  splitbot = import ../default.nix;
in pkgs.dockerTools.buildLayeredImage {
  name = imageName;
  inherit tag;
  contents = [
    pkgs.cacert
    splitbot
  ];

  config = {
    Env = [
      "LC_ALL=C.UTF-8"
    ];
    Cmd = [ "/bin/splitbot" ];
    WorkingDir = "/";
  };
}
