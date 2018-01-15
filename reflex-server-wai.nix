let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-server-wai = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-server-wai";
      rev = "2ff8d9509b4714eb81960c699cb9cd920b568043";
      sha256 = "0h2ld5ksdzbq51wi8sbzaswiy8ngmd4j0id134x621033y3s0kpb";
    };
  };
in
  sources.reflex-server-wai

