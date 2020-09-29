module GameOfLife.Core
    ( CellState (..)
    , Grid (..)
    , cellState
    , countAliveCellsInList
    , countAliveNeighboursOnGrid
    , gridState
    , neighboursInWindow
    , paddedGrid
    ) where

import qualified Data.List
import GameOfLife.SlidingWindow

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
gridState grid =
    let neighbours = countAliveNeighboursOnGrid grid
        gridList = getGridList grid
        zippedStateAndNeighNum = zipWith zip gridList neighbours
    in Grid $ map (map (uncurry cellState)) zippedStateAndNeighNum

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

countAliveCellsInList :: [CellState] -> Int
countAliveCellsInList = length . filter (Alive ==)

countAliveNeighboursOnGrid :: Grid -> [[Int]]
countAliveNeighboursOnGrid grid =
    let pGrid = paddedGrid grid
        windows = slidingWindow2d 3 (getGridList pGrid)
        neighbours = map (map neighboursInWindow) windows
    in map (map countAliveCellsInList) neighbours
