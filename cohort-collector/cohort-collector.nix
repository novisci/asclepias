{ mkDerivation, aeson, amazonka, amazonka-s3, base, bytestring
, conduit, hasklepias-appBuilder, hasklepias-core, lib
, optparse-applicative, tasty, tasty-silver, text
}:
mkDerivation {
  pname = "cohort-collector";
  version = "0.20.3";
  src = ./cohort-collector;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-s3 base bytestring conduit
    hasklepias-appBuilder hasklepias-core optparse-applicative text
  ];
  executableHaskellDepends = [
    base hasklepias-appBuilder hasklepias-core
  ];
  testHaskellDepends = [ base bytestring tasty tasty-silver ];
  homepage = "https://github.com/novisci/asclepias/#readme";
  description = "embedded DSL for defining epidemiologic cohorts";
  license = lib.licenses.bsd3;
}