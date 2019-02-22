{ mkDerivation, aeson, base, containers, freer-simple, hspec
, servant, servant-server, stdenv, tagged, text, wai, warp
}:
mkDerivation {
  pname = "haskell-web-app-freer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers freer-simple servant servant-server tagged
    text wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
