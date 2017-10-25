let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    server-reflex = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo = "servant-reflex";
      rev = "4f77a91a35ddd89c0ac2ecefb1c1e115ad86c460";
      sha256 = "100ga6sd316zx07l2dxn269kxa0b16xsir08wv2h7y93apiraabj";
    };
  };
in
  sources.server-reflex
