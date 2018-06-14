{ mkDerivation, aeson, base, bytestring, containers, hspec, jwt
, mtl, optparse-applicative, random, stdenv, text, time, uuid
}:
mkDerivation {
  pname = "capabilities";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers jwt mtl random text time uuid
  ];
  executableHaskellDepends = [
    aeson base bytestring jwt mtl optparse-applicative text time
  ];
  testHaskellDepends = [ base hspec jwt mtl time ];
  homepage = "https://github.com/luminescent-dreams/capabilities#readme";
  license = stdenv.lib.licenses.bsd3;
}
