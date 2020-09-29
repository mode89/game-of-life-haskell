module GameOfLife.SlidingWindow
    ( slidingWindow
    , slidingWindowThroughColumns
    , slidingWindow2d
    ) where

import qualified Data.List as List

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow windowSize xs =
    let listLenght = List.length xs
        windowNum = listLenght - (windowSize - 1)
    in take windowNum . List.transpose . take windowSize . List.tails $ xs

slidingWindowThroughColumns :: Int -> [[a]] -> [[[a]]]
slidingWindowThroughColumns windowSize grid =
    List.transpose $ map (slidingWindow windowSize) grid

slidingWindow2d :: Int -> [[a]] -> [[[[a]]]]
slidingWindow2d windowSize grid =
    let rowWindows = slidingWindow windowSize grid
    in map (slidingWindowThroughColumns windowSize) rowWindows
