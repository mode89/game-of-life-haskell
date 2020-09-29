import GameOfLife
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Tests" [ testUnderpopulationRule ]

testUnderpopulationRule = testCase "Underpopulation rule" $
    let liveNeighboursNumber = 0
        prevCellState = Alive
        newCellState = cellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must die" newCellState Dead
