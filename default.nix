{ build_demo ? false,
  reflex-platform ? import ./reflex-platform.nix
}:

# (abort (builtins.concatStringsSep " " (builtins.attrNames reflex-platform))) ( {pkgs, ...}: {
reflex-platform.project ({ pkgs, ... }: {
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
        fixSrc   = p: p.overrideAttrs (oldAttrs:  {
          src = builtins.filterSource (
            path: type: baseNameOf path != "integration-tests" &&
                        baseNameOf path != "result"
          ) ./.;
                   });
      in
        fixSrc (setFlags (pkgs.haskell.lib.addBuildDepends super.pdfjs extraDeps));
  };

  shells = {
    ghc   = ["pdfjs" ];
    ghcjs = ["pdfjs" ];
  };
})
