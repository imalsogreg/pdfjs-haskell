(import ./reflex-platform.nix).project ({ pkgs, ... }: {
  packages = {
    pdfjs = ./.;
  };
  overrides = self : super : {
    
  };

  shells = {
    ghc   = ["pdfjs" ];
    ghcjs = ["pdfjs" ];
  };
})
