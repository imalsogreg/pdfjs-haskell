{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}

module GHCJS.DOM.PDF where

import           Control.Lens
import qualified Control.Monad.IO.Class                 as IO
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString                        as ByteString
import qualified Data.Default as Default
import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import           GHC.Generics                           (Generic)
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core


renderPage :: Text.Text -> Int -> JSM ()
renderPage pdfBase64 pageNum = do
  let
    j1 = js1 @Text.Text
    log logArg = jsg @String "console" ^. j1 "log" logArg
  -- log "renderPage"
  syncPoint
  pdfBytes <- jsg @String "window" ^. j1 "atob" pdfBase64
  docData  <- do
    obj <- create
    bytes <- toJSVal pdfBytes
    setProp "data" bytes obj
    return obj
  document <- jsg @String "pdfjsLib" ^. js1 @String "getDocument" docData
  p        <- getProp "promise" =<< valToObject document
  result   <- p ^. js1 @String "then"
               (fun $ \_ _ [pdf] -> do
                   log ("pdf" :: String)
                   render <- pdf ^. j1 "getPage" pageNum
                   render ^. j1 "then" (fun $ \_ _ [page] -> do
                         let
                           scale = 1.5 :: Double
                         viewportArg <- valMakeJSON
                                        (Aeson.object ["scale" Aeson..= scale])
                         viewport <- page ^. j1 "getViewport" viewportArg
                         canvas <- jsg @String "document" ^. j1 "getElementById" ("the-canvas" :: String)
                         context <- canvas ^. j1 "getContext" ("2d" :: String)
                         renderContext <- do
                           rc <- create
                           setProp "canvasContext" context  rc
                           setProp "viewport"      viewport rc
                           return rc
                         page ^. j1 "render" renderContext
                         return ())
                   return ()
               )
  return ()
