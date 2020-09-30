{-# LANGUAGE OverloadedStrings #-}

module GameOfLife.UI.Gtk ( main ) where

import Control.Concurrent
import Data.GI.Base
import GameOfLife.Core
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as GtkEnums

main :: IO ()
main = do
    Gtk.init Nothing
    window <- Gtk.windowNew GtkEnums.WindowTypeToplevel
    Gtk.windowSetTitle window "Game of Life"
    Gtk.onWidgetDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.main
