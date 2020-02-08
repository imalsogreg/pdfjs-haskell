{ runCommand
, lib
, python3
, geckodriver
, firefox-devedition-bin
}:

let pyEnv = python3.withPackages (ps: [ps.selenium]);
    pdfjs = (import ../default.nix { build_demo = true; }).ghcjs.pdfjs;
in
runCommand "runTests.sh"
  {
    buildInputs = [geckodriver pyEnv firefox-devedition-bin];
    src = ./.;
    inherit pdfjs;
    firefox = firefox-devedition-bin;
  } ''
echo TODO
''
