{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hashable, http-types, prometheus
      , servant, servant-multipart, servant-rawm, stdenv, text, time
      , unordered-containers, wai
      }:
      mkDerivation {
        pname = "servant-prometheus";
        version = "0.0.1";
        src = ./.;
        libraryHaskellDepends = [
          base hashable http-types prometheus servant servant-multipart
          servant-rawm text time unordered-containers wai
        ];
        description = "Helpers for using promethues with servant";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
