{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghc"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  haskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: {
      reflex-binary = self.callPackage (import ../reflex-binary.nix) {inherit reflex-platform compiler;};
    };
  };
  drv = haskellPackages.callPackage ./common.nix { };
in
  drv
