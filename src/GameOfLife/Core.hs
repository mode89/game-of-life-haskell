module GameOfLife.Core
    ( CellCoord (..)
    , CellState (..)
    , Grid (..)
    , cellCoordFromGrid
    , countAliveCellsInList
    , countAliveNeighboursOnGrid
    , emptyGrid
    , embedGrid
    , nextCellState
    , nextGridState
    , neighboursInWindow
    , paddedGrid
    , rPentomino
    , stringFromGrid
    ) where

import qualified Data.List as List
import Data.Maybe
import GameOfLife.SlidingWindow

data CellCoord = CellCoord
    {
        getCellCoordX :: Int,
        getCellCoordY :: Int
    } deriving (Eq, Show)
data CellState = Dead | Alive deriving (Eq, Show)
newtype Grid = Grid { getGridList :: [[CellState]] } deriving (Eq, Show)

nextCellState :: CellState -> Int -> CellState
nextCellState Alive liveNeighboursNumber
    | liveNeighboursNumber < 2 = Dead
    | liveNeighboursNumber > 3 = Dead
    | otherwise = Alive
nextCellState Dead liveNeighboursNumber
    | liveNeighboursNumber == 3 = Alive
    | otherwise = Dead

nextGridState :: Grid -> Grid
nextGridState grid =
    let neighbours = countAliveNeighboursOnGrid grid
        gridList = getGridList grid
        zippedStateAndNeighNum = zipWith zip gridList neighbours
    in Grid $ map (map (uncurry nextCellState)) zippedStateAndNeighNum

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
    window !! 0 ++ [window !! 1 !! 0, window !! 1 !! 2] ++ window !! 2

countAliveCellsInList :: [CellState] -> Int
countAliveCellsInList = length . filter (Alive ==)

countAliveNeighboursOnGrid :: Grid -> [[Int]]
countAliveNeighboursOnGrid grid =
    let pGrid = paddedGrid grid
        windows = slidingWindow2d 3 (getGridList pGrid)
        neighbours = map (map neighboursInWindow) windows
    in map (map countAliveCellsInList) neighbours

stringFromGrid :: Grid -> String
stringFromGrid grid =
    List.intercalate "\n" . map stringFromCellList $ getGridList grid

stringFromCell :: CellState -> String
stringFromCell Alive = "*"
stringFromCell Dead = "."

stringFromCellList :: [CellState] -> String
stringFromCellList cells = concat $ map stringFromCell cells

emptyGrid :: Int -> Int -> Grid
emptyGrid rows columns = Grid $ replicate rows $ replicate columns Dead

rPentomino :: Grid
rPentomino = Grid [[  Dead, Alive, Alive ],
                   [ Alive, Alive,  Dead ],
                   [  Dead, Alive,  Dead ]]

embedGrid :: Grid -> Int -> Int -> Grid -> Grid
embedGrid baseGrid row column grid = Grid $ header ++ body ++ footer where
    header = take row $ getGridList baseGrid
    body = zipWith bodyRow baseGridBodyRows (getGridList grid)
    footer = drop (headerHeight + bodyHeight) $ getGridList baseGrid
    headerHeight = row
    bodyHeight = length $ getGridList grid
    bodyRow baseRow row = beginning ++ row ++ ending where
        beginning = take column baseRow
        ending = drop (column + rowWidth) baseRow
        rowWidth = length row
    baseGridBodyRows = take bodyHeight . drop row $ getGridList baseGrid

cellCoordFromGrid :: Grid -> [CellCoord]
cellCoordFromGrid grid =
    catMaybes $ map maybeAliveCell $ enumAllGridCells grid where
        maybeAliveCell (cellCoord, cellState) =
            case cellState of
                Alive -> Just $ cellCoord
                Dead ->  Nothing

enumAllGridCells :: Grid -> [(CellCoord, CellState)]
enumAllGridCells grid = concat $ zipWith enumRows [0..] gridRows where
    gridRows = getGridList grid
    enumRows rowIndex row = zipWith (enumRowCells rowIndex) [0..] row
    enumRowCells rowIndex columnIndex cellState =
        (CellCoord rowIndex columnIndex, cellState)
