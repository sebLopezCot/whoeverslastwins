{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }) {}
}:
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjsHEAD ghc802;
  miso-src = pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "0xcbyphv2wxj6r8n66003w13x3kfw0sk3qdrf331mrz768p564v5";
    rev = "8d115ab0c04c3f5a41497a6a460b8cc1656a975d";
  };
  miso-ghc = ghc802.callCabal2nix "miso" miso-src {};
  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};
  api = ghc802.callPackage ./api {
    datetime = pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.datetime);
  };
  server = ghc802.callPackage ./server {
    datetime = pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.datetime);
    wlw-api = api;
  };
  web-src = ./web;
  web = ghcjsHEAD.callPackage web-src {
    miso = miso-ghcjs;
  };
in
  runCommand "whoeverslastwins" { inherit web server; } ''
    mkdir -p $out/wlw-web
    cp ${web-src}/web/index.html $out/wlw-web/index.html
    cp ${web-src}/web/style.css $out/wlw-web/style.css
    cp ${server}/bin/wlw-server $out/wlw-server
    ${closurecompiler}/bin/closure-compiler ${web}/bin/wlw-web.jsexe/all.js > $out/wlw-web/script.js
  ''
