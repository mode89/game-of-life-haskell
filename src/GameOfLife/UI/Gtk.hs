{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module GameOfLife.UI.Gtk ( main ) where

import Control.Concurrent
import Data.GI.Base
import GameOfLife.Core
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
    Gtk.init Nothing
    win <- new Gtk.Window [#title := "Introduction"]
    on win #destroy Gtk.mainQuit
    #showAll win
    Gtk.main
