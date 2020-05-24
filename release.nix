{ nixpkgs, reflex-platform }:

{
  pdfjs-haskell = import ./default.nix { inherit reflex-platform; };
}
