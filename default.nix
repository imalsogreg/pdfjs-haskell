{build_demo ? false}:

(import ./reflex-platform.nix).project ({ pkgs, ... }: {
  packages = {
    pdfjs = ./.;
  };
  overrides = self : super : {
    pdfjs =
      let
        extraDeps = if   self.ghc.isGhcjs or false
                    then (if build_demo then [self.reflex-dom] else [])
                    else [self.reflex-dom self.jsaddle-webkit2gtk self.jsaddle-warp];
        setFlags = p: if   build_demo
                      then pkgs.haskell.lib.appendConfigureFlag p "-f Demo"
                      else p;
      in
        setFlags (pkgs.haskell.lib.addBuildDepends super.pdfjs extraDeps);
  };

  shells = {
    ghc   = ["pdfjs" ];
    ghcjs = ["pdfjs" ];
  };
})
