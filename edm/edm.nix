{ mkDerivation, aeson, autoexporter, base, bytestring, containers
, contravariant, generic-lens, hspec, interval-algebra, lib
, microlens, QuickCheck, text, time, vector, witherable 
}:
mkDerivation {
  pname = "edm";
  version = "0.23.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers contravariant generic-lens
    interval-algebra microlens QuickCheck text time vector witherable
  ];
  libraryToolDepends = [ autoexporter ];
  testHaskellDepends = [
    aeson base bytestring containers hspec interval-algebra text time
  ];
  homepage = "https://github.com/novisci/asclepias/#readme";
  description = "event data model";
  license = lib.licenses.bsd3;
}
