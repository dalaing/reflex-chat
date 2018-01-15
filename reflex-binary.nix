let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-binary = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-binary";
      rev = "add328aebbe27485e14dea05e8b506f36934833a";
      sha256 = "0xvrdjdssnrlnwji9nrzzjvr1fwgis5wdzb29bwx2xc2vlhrw3jz";
    };
  };
in
  sources.reflex-binary

