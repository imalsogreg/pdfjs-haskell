{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import <reflex-platform> {}
}:

let pdfjs-package = import ./default.nix { inherit reflex-platform; };
in
{
  pdfjs = pdfjs-package.ghc.pdfjs;
}
