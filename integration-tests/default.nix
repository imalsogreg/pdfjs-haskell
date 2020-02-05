{ runCommand, lib, python3, geckodriver, firefox, firefox-devedition-bin, xvfb_run, xdpyinfo, xorg }:

let pyEnv = python3.withPackages (ps: [ps.selenium ps.virtual-display ps.xvfbwrapper]);
    pdfjs = (import ../default.nix { build_demo = true; }).ghcjs.pdfjs;
in
runCommand "runTests.sh"
  {
    buildInputs = [geckodriver pyEnv xdpyinfo xorg.xorgserver];
    src = ./.;
  } ''
echo Starting server
cd ${pdfjs}/bin/demo.jsexe & ${pyEnv}/bin/python -m http.server &
# export DISPLAY=:0
sleep 1
echo `ls`
cp $src/expected_canvas_bytes.txt ./

echo About to run tests
echo `geckodriver --version`
echo FFVersion `${firefox-devedition-bin}/bin/firefox-devedition --version`
# ${xvfb_run}/bin/xvfb-run echo Display in xvfb_run: $DISPLAY
# ${xvfb_run}/bin/xvfb-run echo Display in xvfb_run2: $DISPLAY
${xvfb_run}/bin/xvfb-run ${pyEnv}/bin/python $src/test.py ${firefox}/bin/firefox $src/smile.pdf
# ${pyEnv}/bin/python $src/test.py ${firefox-devedition-bin}/bin/firefox-devedition $src/smile.pdf &
echo STILL ALIVE
cat geckodriver.log
echo PRINTED LOG
''
