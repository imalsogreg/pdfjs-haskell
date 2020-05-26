-- |

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module GHCJS.DOM.PDF.Internal where

import qualified Control.Concurrent           as Concurrent
import           Control.Concurrent.STM       (atomically)
import qualified Control.Concurrent.STM.TMVar as TMVar
import           Control.Lens                 ((^.))
import qualified Control.Monad                as Monad
import qualified Control.Monad.IO.Class       as IO
import qualified Data.Aeson                   as Aeson
import qualified Data.Text                    as Text
import           Language.Javascript.JSaddle
import qualified Language.Javascript.JSaddle  as JS

newtype PDF = PDF { pdfValue :: JSVal }

dataUrlPrefix :: Text.Text
dataUrlPrefix = Text.pack "data:application/pdf;base64,"

getPDFFromBareBase64Bytestring :: Text.Text -> JSM PDF
getPDFFromBareBase64Bytestring pdfBase64 = do
  let
    j1 = js1 @String
    -- log logArg = jsg "console" ^. js1 "log" logArg
  syncPoint
  pdfBytes <- jsg @String "window" ^. j1 "atob" pdfBase64
  docData  <- do
    obj <- create
    bytes <- toJSVal pdfBytes
    setProp "data" bytes obj
    return obj
  IO.liftIO $ Concurrent.threadDelay 2000000
  pdfjsLib <- jsg @String "pdfjsLib"
  pdfjsLibUndefined <- JS.ghcjsPure $ JS.isUndefined pdfjsLib
  Monad.when pdfjsLibUndefined
    (error "pdfjs-haskell error: pdfjsLib is undefined")
  document <- jsg @String "pdfjsLib" ^. js1 @String "getDocument" docData
  p        <- getProp ("promise" :: JSString) =<< valToObject document
  resultVar <- IO.liftIO TMVar.newEmptyTMVarIO
  p ^. js1 @String "then"
               (fun $ \_ _ [pdf] -> do
                   IO.liftIO $ atomically $ TMVar.putTMVar resultVar pdf
               )
  result <- IO.liftIO $ atomically (TMVar.readTMVar resultVar)
  return $ PDF result


data PageDimensions = PageDimensions
  { pageTop    :: Int
  , pageLeft   :: Int
  , pageWidth  :: Int
  , pageHeight :: Int
  } deriving (Eq, Show)

data RenderResult
  = GoodRender PageDimensions
  | NoCanvas
  deriving (Show, Eq)

renderPDFPage
  :: PDF
  -> Int
  -> String
  -> Double
  -> JSM RenderResult
renderPDFPage (PDF jsPDF) pageNum canvasName scale = do
  let log logArg = jsg @String "console" ^. js1 ("log" :: String) logArg
  resultVar <- IO.liftIO TMVar.newEmptyTMVarIO
  render <- jsPDF ^. js1 ("getPage" :: String) pageNum
  render ^. js1 ("then" :: String)
    (fun $ \_ _ [page] -> do
        viewportArg <- valMakeJSON
                       (Aeson.object [Text.pack "scale" Aeson..= scale])
        viewport <- page ^. js1 ("getViewport" :: String) viewportArg
        canvas <- jsg @String "document" ^. js1 ("getElementById" :: String) canvasName

        height <- viewport ! ("height" :: String)
        width  <- viewport ! ("width"  :: String)
        canvas ^. js2 ("setAttribute" :: String) ("height" :: String) height
        canvas ^. js2 ("setAttribute" :: String) ("width"  :: String) width

        context <- canvas ^. js1 ("getContext" :: String) ("2d" :: String)
        renderContext <- do
          rc <- create
          setProp ("canvasContext") context  rc
          setProp ("viewport")      viewport rc
          return rc
        page ^. js1 ("render" :: String) renderContext
        pageView <- page ! ("view" :: String)
        pageDimensions <- PageDimensions
          <$> (fromJSValUnchecked =<< pageView JS.!! 0)
          <*> (fromJSValUnchecked =<< pageView JS.!! 1)
          <*> (fromJSValUnchecked =<< pageView JS.!! 2)
          <*> (fromJSValUnchecked =<< pageView JS.!! 3)
        IO.liftIO $ atomically $ TMVar.putTMVar resultVar $ GoodRender pageDimensions
    )
  IO.liftIO $ atomically $ TMVar.readTMVar resultVar
