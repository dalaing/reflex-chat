let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-server-servant = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-server-servant";
      rev = "c8c442c73d4e3e0fdd2672247c2947c1bc4485cb";
      sha256 = "1gipfbg4px2inpk751nvws5ffyjrq84rpdbjn0qgjv9y7zi5xnn1";
    };
  };
in
  sources.reflex-server-servant
