{ reflex-platform ? import ../reflex-platform.nix
} : 
let
  drv = import ./. { inherit reflex-platform; };
in
  drv.env
