{ mkDerivation, base, bcrypt, bytestring, datetime, entropy
, monad-logger, mtl, persistent, persistent-sqlite, servant
, servant-server, stdenv, warp, wlw-api
}:
mkDerivation {
  pname = "wlw-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bcrypt bytestring datetime entropy monad-logger mtl persistent
    persistent-sqlite servant servant-server warp wlw-api
  ];
  license = stdenv.lib.licenses.unfree;
}
