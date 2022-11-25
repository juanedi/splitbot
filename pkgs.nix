let
  lockFile = builtins.fromJSON (builtins.readFile ./devenv.lock);
  pkgsSpec = lockFile.nodes.nixpkgs-stable.locked;
  pkgsRev = pkgsSpec.rev;
  pkgsHash = pkgsSpec.narHash;
in
import
  (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${pkgsRev}.tar.gz";
    sha256 = pkgsHash;
  })
