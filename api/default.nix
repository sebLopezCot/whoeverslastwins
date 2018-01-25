{ mkDerivation, aeson, base, datetime, persistent
, persistent-template, servant-server, stdenv
}:
mkDerivation {
  pname = "wlw-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base datetime persistent persistent-template servant-server
  ];
  license = stdenv.lib.licenses.unfree;
}
