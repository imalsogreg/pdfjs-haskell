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
import           Data.FileEmbed                         (embedFile)
import qualified Data.Default as Default
import qualified Data.Text                              as Text
import qualified Data.Text.Encoding                     as Text
import           GHC.Generics                           (Generic)
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.WebSockets
#endif


newtype PageNumber = PageNumber { getPageNumber :: Int }
  deriving( Eq, Generic, Show )


-- | A viewer is defined by the Reflex action for creating
-- a rendering canvas, and an Event of requests for page
-- numbers to render
data PDFViewConfig t m = PDFViewConfig
  { makeCanvasElement :: m (El t)
  , render            :: Event t (Maybe PageNumber)
  }


instance
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  ) => Default.Default (PDFViewConfig t m) where
  def = PDFViewConfig
    { makeCanvasElement = do
        let
          attrs =
            "width"  =: "500px" <>
            "height" =: "400px" <>
            "id"     =: "pdfjs-canvas"
        (el, _) <- elAttr' "canvas" attrs (return ())
        return el
    , render            = never
    }


data RenderStatus
  = Loading
  | Rendering
  | Error Text.Text
  | Ready
  deriving (Eq, Ord, Generic, Show)


data PDFView t = PDFView
  { canvas       :: El t
  , renderStatus :: Dynamic t RenderStatus
  }

run :: JSM ()
run =
  mainWidgetWithHead widgetHead $ do
  b <- button (Text.pack "Click")
  canvas <- elAttr "canvas" ("id" =: "the-canvas" <>
                             "width" =: "300px" <>
                             "height" =: "400px" <>
                             "style" =: "box-shadow:5px 5px 5px black;") (return ())
  performEvent $ ffor b
    (\() -> liftJSM $ do
        renderPage examplePdf 2
        return ()
    )
  return ()
    where
      widgetHead :: (DomBuilder t m) => m ()
      widgetHead = do
        script "https://cdnjs.cloudflare.com/ajax/libs/pdf.js/2.3.200/pdf.js"
        -- scriptInline s1
      script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank
      -- scriptInline s = elAttr "script" ("type" =: "text/javascript") (text s)

examplePdf :: Text.Text
examplePdf =
  Text.decodeUtf8 $(embedFile "/home/greghale/Documents/astro_64.pdf")


renderPage :: Text.Text -> Int -> JSM ()
renderPage pdfBase64 pageNum = do
  let
    j1 = js1 @Text.Text
    log logArg = jsg @String "console" ^. j1 "log" logArg
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
