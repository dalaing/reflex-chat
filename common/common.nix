{ mkDerivation, aeson, base, binary, reflex-binary, servant, stdenv
}:
mkDerivation {
  pname = "common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary reflex-binary servant
  ];
  license = stdenv.lib.licenses.bsd3;
}
