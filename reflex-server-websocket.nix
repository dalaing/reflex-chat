let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-server-websocket = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-server-websocket";
      rev = "2f9d6a3befaa3a938430ec0cd7179a442fe5276f";
      sha256 = "0aiw7595jq4ff7x15hcarkmrmw3j3x6xvf8glrwyh935avk99jg3";
    };
  };
in
  sources.reflex-server-websocket

