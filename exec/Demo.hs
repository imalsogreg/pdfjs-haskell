{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import qualified Control.Monad.IO.Class                 as IO
import           Data.Default                           (Default, def)
import qualified Data.Text                              as Text

import qualified Data.Text.Encoding                     as Text
import qualified GHCJS.DOM.FileReader                   as FileReader

import qualified Data.JSString.Text                     as JSS
import qualified GHCJS.DOM.Types                        as DOM
import           Language.Javascript.JSaddle            (JSM, MonadJSM, liftJSM)
import qualified Language.Javascript.JSaddle            as JSaddle
import           Reflex.Dom
import qualified Reflex.Dom.Main                        as ReflexMain

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.WebSockets (debug)
#endif

import qualified GHCJS.DOM.PDF                          as PDF

app
  :: forall t m .
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , PerformEvent t m
  , MonadHold t m
  , TriggerEvent t m
  , MonadJSM m
  , PostBuild t m
  , MonadJSM (Performable m)
  ) => m ()
app = do

  (b,f) <- el "div" $ do
    b <- fmap (domEvent Click . fst) $
         elAttr' "button" ("id" =: "render") $ text (Text.pack "Render P1")

    f <- inputElement $ InputElementConfig
      { _inputElementConfig_elementConfig = ElementConfig
        { _elementConfig_namespace = Nothing
        , _elementConfig_initialAttributes = "type" =: "file" <> "id" =: "choose-file"
        , _elementConfig_modifyAttributes = Nothing
        , _elementConfig_eventSpec = def
        }
      , _inputElementConfig_initialValue = ""
      , _inputElementConfig_setValue = Nothing
      , _inputElementConfig_initialChecked = False
      , _inputElementConfig_setChecked = Nothing
      }
    return (b,f)

  let files = _inputElement_files f
  let fileAsyncReads = ffor (updated files) $ \f -> \tell -> liftJSM $ do
        reader <- FileReader.newFileReader
        FileReader.readAsDataURL reader (Just (head f))
        let readerElement = DOM.uncheckedCastTo DOM.HTMLElement reader
        liftJSM $ elementOnEventName Load readerElement $ do
          stringOrBuffer <-  FileReader.getResultUnchecked reader
          pdfData <- liftJSM $ JSaddle.ghcjsPure $ JSS.textFromJSVal $ DOM.pToJSVal stringOrBuffer
          pdf <- liftJSM $ PDF.fromDataUrlBase64 pdfData
          IO.liftIO $ tell (Just pdf)
        return ()


  loadedFile <- holdDyn Nothing =<< performEventAsync fileAsyncReads

  canvas1 <- el "div" $ elAttr "canvas" ("id" =: "the-canvas1" <>
                                         "width" =: "300px" <>
                                         "height" =: "300px" <>
                                         "style" =: "box-shadow:5px 5px 5px black;")
                        (return ())
  canvas2 <- el "div" $ elAttr "canvas" ("id" =: "the-canvas2" <>
                                         "width" =: "300px" <>
                                         "height" =: "300px" <>
                                         "style" =: "box-shadow:5px 5px 5px black;")
                        (return ())

  res <- performEvent $ ffor (tagPromptlyDyn loadedFile b) $ \case
    Nothing  -> return (Left "No PDF loaded", Left "No PDF loaded")
    Just pdf -> liftJSM $ (,)
      <$> fmap Right (PDF.renderPage' pdf 1 "the-canvas1" 1.0)
      <*> fmap Right (PDF.renderPage' pdf 2 "the-canvas2" 0.5)

  el "div" $ display =<< holdDyn (Left "waiting") (fst <$> res)
  el "div" $ display =<< holdDyn (Left "waiting") (snd <$> res)
  return ()

widgetHead :: (DomBuilder t m) => m ()
widgetHead =
  script "https://cdnjs.cloudflare.com/ajax/libs/pdf.js/2.3.200/pdf.js"
  where
        -- scriptInline s1
    script src = elAttr "script" ("type" =: "text/javascript" <> "src" =: src) blank
      -- scriptInline s = elAttr "script" ("type" =: "text/javascript") (text s)

main :: IO ()
main =
#ifdef ghcjs_HOST_OS
  ReflexMain.mainWidgetWithHead widgetHead app
#else
  debug 8080 (ReflexMain.mainWidgetWithHead widgetHead app)
#endif
