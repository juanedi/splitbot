let
  lockFile = builtins.fromJSON (builtins.readFile ./devenv.lock);
  pkgsSpec = lockFile.nodes.nixpkgs-stable.locked;
in
import
  (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${pkgsSpec.rev}.tar.gz";
    sha256 = pkgsSpec.narHash;
  })
