{ mkDerivation, aeson, async, base, bytestring, containers
, directory, filepath, http-client, http-client-tls, http-types
, lib, mtl, scotty, stm, text, trifecta, wai-extra
}:
mkDerivation {
  pname = "splitbot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring containers directory filepath
    http-client http-client-tls http-types mtl scotty stm text trifecta
    wai-extra
  ];
  license = "unknown";
  mainProgram = "splitbot";
}
