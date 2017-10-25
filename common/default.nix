{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghc"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  haskellPackages = reflex-platform.${compiler};
  drv = haskellPackages.callPackage ./common.nix { };
in
  drv
