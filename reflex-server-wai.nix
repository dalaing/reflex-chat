let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-server-wai = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-server-wai";
      rev = "f4a2a087041e2f12701b1b14c40c9905ebe5cb8c";
      sha256 = "0gj4jjl3rrw0qkqjys3k1x35wnv559dqw3762bb1ndngca9zk1x4";
    };
  };
in
  sources.reflex-server-wai

