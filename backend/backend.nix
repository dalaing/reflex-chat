{ mkDerivation, base, bytestring, common, containers, filepath, mtl
, reflex, reflex-basic-host, reflex-server-servant
, reflex-server-wai, reflex-server-websocket, servant
, servant-server, stdenv, stm, text, these, wai, wai-app-static
, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring common containers filepath mtl reflex
    reflex-basic-host reflex-server-servant reflex-server-wai
    reflex-server-websocket servant servant-server stm text these wai
    wai-app-static wai-websockets warp websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
