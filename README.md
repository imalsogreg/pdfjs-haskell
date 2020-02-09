pdfjs-haskell
===

[![Build Status](https://travis-ci.org/imalsogreg/pdfjs-haskell.svg?branch=master)](https://travis-ci.org/imalsogreg/pdfjs-haskell)

jsaddle/ghcjs wrapper around [pdf.js](https://github.com/mozilla/pdf.js)

Uploaded to github for personal use. Not ready for public consumption.

## Try it out

The demo app is behind a cabal flag, so that it doesn't get shipped with
the library. Turn the flag on and run with
[jsaddle-warp](https://github.com/ghcjs/jsaddle)
like so:

``` shell
nix-shell -A shells.ghc
cabal v2-repl -f Demo exe:demo
*Main> main
<a href="http://localhost:8080">run</a>
```

Or compile it to javascript:

``` shell
nix build -f default.nix ghcjs.pdfjs --arg build_demo true
nix-shell -p python3 --run cd result/bin/demo.jsexe && python -m http.server 8000"
```
