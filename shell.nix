{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;
 
  f = { mkDerivation, aeson, base, beam-core, beam-postgres
      , bytestring, containers, dhall, freer-simple, hspec, lens
      , optparse-applicative, postgresql-simple, resource-pool, scrypt
      , servant, servant-server, stdenv, tagged, text, time
      , transformers-base, wai, warp
      }:
      mkDerivation {
        pname = "haskell-web-app-freer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base beam-core beam-postgres bytestring containers dhall
          freer-simple lens postgresql-simple resource-pool scrypt servant
          servant-server tagged text time transformers-base wai warp
        ];
        executableHaskellDepends = [ base optparse-applicative ];
        testHaskellDepends = [ base freer-simple hspec ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  freer-simple = haskellPackages.callPackage ./deps/freer-simple.nix { };

  drv = variant (haskellPackages.callPackage f { inherit freer-simple; });

in

  if pkgs.lib.inNixShell then drv.env else drv
