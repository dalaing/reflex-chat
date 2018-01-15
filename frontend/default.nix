{ reflex-platform ? import ../reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  haskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: {
      common = import ../common { inherit reflex-platform compiler; };
      servant-reflex = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant-reflex" (import ../servant-reflex.nix) {});
    };
  };
  drv = pkgs.haskell.lib.dontHaddock (haskellPackages.callPackage ./frontend.nix { });
in
  drv
