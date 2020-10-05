{-# LANGUAGE OverloadedStrings #-}

module GameOfLife.UI.Gtk ( main ) where

import Control.Concurrent
import Control.Monad.Trans.Reader (runReaderT)
import Data.GI.Base
import Data.IORef
import Foreign.Ptr (castPtr)
import GameOfLife.Core
import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as GtkEnums
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

main :: IO ()
main = do
    gridRef <- newIORef $ embedGrid (emptyGrid 101 101) 49 49 rPentomino
    Gtk.init Nothing
    window <- Gtk.windowNew GtkEnums.WindowTypeToplevel
    Gtk.windowSetTitle window "Game of Life"
    Gtk.onWidgetDestroy window Gtk.mainQuit
    drawingArea <- Gtk.drawingAreaNew
    Gtk.onWidgetDraw drawingArea $
        \context -> do
            grid <- readIORef gridRef
            renderWithContext context $ do
                renderGrid grid
            return True
    box <- Gtk.boxNew GtkEnums.OrientationVertical 0
    Gtk.boxPackStart box drawingArea True True 0
    Gtk.containerAdd window box
    Gtk.widgetShowAll window
    GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 $ do
        modifyIORef gridRef nextGridState
        Gtk.widgetQueueDraw drawingArea
        return True
    Gtk.main

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext context render = withManagedPtr context $
    \pointer ->
        runReaderT (runRender render) (Cairo (castPtr pointer))

renderGrid :: Grid -> Render()
renderGrid grid = do
    setSourceRGB 0 0 0
    identityMatrix
    scale 10 10
    mapM_ renderCell $ cellCoordFromGrid grid
    fill

renderCell :: CellCoord -> Render ()
renderCell cellCoord = do
    rectangle x y 1 1
    where
        x = fromIntegral $ getCellCoordX cellCoord
        y = fromIntegral $ getCellCoordY cellCoord
