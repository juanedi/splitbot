let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  baseDeps = ps:
    with ps; [
      base
      bytestring
      containers
      http-types
      http-client
      http-client-tls
      aeson
      trifecta
      text
      wai-extra
      mtl
      stm
      scotty
      async
      filepath
      directory
    ];
  developmentDeps = ps:
    with ps; [
      hspec
    ];
in with nixpkgs.haskellPackages;
{
  development = ghcWithPackages ( ps : (baseDeps ps) ++ (developmentDeps ps));
  release = ghcWithPackages baseDeps;
}
