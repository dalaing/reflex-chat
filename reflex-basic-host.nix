let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-basic-host = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "b47bd584ef169da362b6500757deb2fddab41f0e";
      sha256 = "1kbrbwbp1jvcijpcqnxd50j3pjwx5bl6sggs7098zwjs3nr0qvla";
    };
  };
in
  sources.reflex-basic-host

