let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-basic-host = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "a06461166895c680153c44de757330c3102ab6d5";
      sha256 = "05a5fc96r1gc8l2q0mnw4xz0lvg7hzdw7i5ssrfxrcgikngmajyn";
    };
  };
in
  sources.reflex-basic-host

