let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    server-reflex = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo = "servant-reflex";
      rev = "153d3254bef9d6eb4c05647945df281a87e5e649";
      sha256 = "1mv15zym9114vkw043rn9ld5q9npayaq42mw0dc45lbpqnfb6wr1";
    };
  };
in
  sources.server-reflex
