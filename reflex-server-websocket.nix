let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-server-websocket = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-server-websocket";
      rev = "5e89164fc4121097f7f5058ac12c16f8e68bd3bd";
      sha256 = "03swdhpbhb68fxa1cmyhsg42m7lxvsxr2vbr2z5l971vv1adsc9n";
    };
  };
in
  sources.reflex-server-websocket

