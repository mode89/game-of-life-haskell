module GameOfLife
    ( CellState (..)
    , Grid (..)
    , cellState
    , gridLiveNeighboursNumber
    , gridState
    , slidingWindow
    ) where

import qualified Data.List as List

data CellState = Dead | Alive deriving (Eq, Show)
newtype Grid = Grid { getGridList :: [[CellState]] } deriving (Eq, Show)

cellState :: CellState -> Int -> CellState
cellState Alive liveNeighboursNumber
    | liveNeighboursNumber < 2 = Dead
    | liveNeighboursNumber > 3 = Dead
    | otherwise = Alive
cellState Dead liveNeighboursNumber
    | liveNeighboursNumber == 3 = Alive
    | otherwise = Dead

gridState :: Grid -> Grid
gridState prevGrid = Grid [[ Dead ]]

gridLiveNeighboursNumber :: Grid -> [[Int]]
gridLiveNeighboursNumber grid = [[ 0 ]]

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow windowSize xs =
    let listLenght = List.length xs
        windowNum = listLenght - (windowSize - 1)
    in take windowNum . List.transpose . take windowSize . List.tails $ xs
