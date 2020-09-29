module GameOfLife.Core
    ( CellState (..)
    , Grid (..)
    , cellState
    , gridLiveNeighboursNumber
    , gridState
    , neighboursInWindow
    , paddedGrid
    ) where

import qualified Data.List

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

paddedGrid :: Grid -> Grid
paddedGrid grid =
    let paddedRowF = \row -> [Dead] ++ row ++ [Dead]
        gridList = getGridList grid
        paddedRowLength = length (gridList !! 0) + 2
        paddingRow = take paddedRowLength (repeat Dead)
        paddedRows = map paddedRowF $ gridList
    in Grid $ [paddingRow] ++ paddedRows ++ [paddingRow]

neighboursInWindow :: [[CellState]] -> [CellState]
neighboursInWindow window =
    [
        window !! 0 !! 1,
        window !! 1 !! 2,
        window !! 2 !! 1,
        window !! 1 !! 0
    ]
