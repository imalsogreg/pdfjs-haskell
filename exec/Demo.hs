module Main where

import qualified GHCJS.DOM.PDF as PDF
import Reflex.Dom
import Language.Javascript.JSaddle.WebSockets (debug)

main :: IO ()
main = debug 8080 PDF.run
