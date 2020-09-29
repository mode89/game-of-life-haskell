module GameOfLife
    ( CellState (..)
    , cellState
    ) where

data CellState = Dead | Alive deriving (Eq, Show)

cellState :: CellState -> Int -> CellState
cellState Alive liveNeighboursNumber
    | liveNeighboursNumber < 2 = Dead
    | otherwise = Alive
