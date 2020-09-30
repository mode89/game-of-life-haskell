module Main where

import Control.Concurrent
import GameOfLife.Core

main :: IO ()
main = do
    let initialGrid = embedGrid (emptyGrid 23 79) 10 38 rPentomino
    mainLoop initialGrid

mainLoop :: Grid -> IO ()
mainLoop grid = do
    putStrLn ""
    putStrLn $ stringFromGrid grid
    let nextGrid = nextGridState grid
    threadDelay 100000
    mainLoop nextGrid
