{ mkDerivation, base, common, containers, directory, filepath
, jsaddle, jsaddle-warp, lens, mtl, reflex, reflex-dom-core
, servant, servant-reflex, stdenv, text, wai, wai-middleware-static
, warp, websockets
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base common containers directory filepath jsaddle jsaddle-warp lens
    mtl reflex reflex-dom-core servant servant-reflex text wai
    wai-middleware-static warp websockets
  ];
  executableHaskellDepends = [ base reflex-dom-core ];
  license = stdenv.lib.licenses.bsd3;
}
