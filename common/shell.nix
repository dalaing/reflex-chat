{ reflex-platform ? import ../reflex-platform.nix
, compiler ? "ghc"
} : 
let
  drv = import ./. { inherit reflex-platform compiler; };
in
  drv.env
