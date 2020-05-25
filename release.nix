{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import <reflex-platform> {}
}:

let pdfjs-package = import ./default.nix { };
in
{
  pdfjs = pdfjs-package.ghcjs.pdfjs;
}
