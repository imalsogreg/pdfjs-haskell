let
  reflex-platform-src = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "1568e673b3b3c3407d5736e9f33b85b502482a7f";
    sha256 = "05vz3w6fh0gfhihnzgc7mvlyhgqg5cj4qmzq0f6frvi9drn4v460";
  };
in
import reflex-platform-src {
  nixpkgsFunc = import <nixpkgs>;
}
