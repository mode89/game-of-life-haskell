{-# LANGUAGE OverloadedStrings #-}

module GameOfLife.UI.Gtk ( main ) where

import Control.Concurrent
import Control.Monad.Trans.Reader (runReaderT)
import Data.GI.Base
import Foreign.Ptr (castPtr)
import GameOfLife.Core
import qualified GI.Cairo as GI.Cairo
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as GtkEnums
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

main :: IO ()
main = do
    Gtk.init Nothing
    window <- Gtk.windowNew GtkEnums.WindowTypeToplevel
    Gtk.windowSetTitle window "Game of Life"
    Gtk.onWidgetDestroy window Gtk.mainQuit
    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetSizeRequest drawingArea 500 500
    Gtk.onWidgetDraw drawingArea $
        \context -> do
            renderWithContext context $ do
                setLineWidth 1.0
                setSourceRGB 0 0 0
                scale 100 100
                translate 1.0 1.0
                rectangle 0 0 1 1
                fill
            return True
    box <- Gtk.boxNew GtkEnums.OrientationVertical 0
    Gtk.boxPackStart box drawingArea True True 0
    Gtk.containerAdd window box
    Gtk.widgetShowAll window
    Gtk.main

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext context render = withManagedPtr context $
    \pointer ->
        runReaderT (runRender render) (Cairo (castPtr pointer))
