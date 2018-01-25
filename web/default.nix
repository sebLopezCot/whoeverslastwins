{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "wlw-web";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso ];
  license = stdenv.lib.licenses.unfree;
}
