{ reflex-platform ? import ../reflex-platform.nix
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  haskellPackages = reflex-platform.ghc.override {
    overrides = self: super: {
      common = import ../common { inherit reflex-platform; compiler = "ghc"; };
      reflex-basic-host = self.callPackage (import ../reflex-basic-host.nix) {inherit reflex-platform;};
      reflex-server-servant = self.callPackage (import ../reflex-server-servant.nix) {inherit reflex-platform;};
      reflex-server-wai = self.callPackage (import ../reflex-server-wai.nix) {inherit reflex-platform;};
      reflex-server-websocket = self.callPackage (import ../reflex-server-websocket.nix) {inherit reflex-platform;};
    };
  };
  drv = haskellPackages.callPackage ./backend.nix { };
in
  drv
