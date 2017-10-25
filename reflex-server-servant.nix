let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-server-servant = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-server-servant";
      rev = "38cb69adaedd00bd705d5f183016c46954e4a5fb";
      sha256 = "11hi6mr5rdnn1dvqaj05fclf2994k5bm6hz0pxm3cg2slij47gis";
    };
  };
in
  sources.reflex-server-servant
