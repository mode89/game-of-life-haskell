import GameOfLife
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Tests"
    [ testUnderpopulationRule
    , testOvercrowdingRule
    , testSurvivalRule
    , testBecomeAliveRule
    , testSingularDeadGrid
    ]

testUnderpopulationRule = testCase "Underpopulation rule" $
    let liveNeighboursNumber = 0
        prevCellState = Alive
        newCellState = cellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must die" newCellState Dead

testOvercrowdingRule = testCase "Overcrowding rule" $
    let liveNeighboursNumber = 4
        prevCellState = Alive
        newCellState = cellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must die" newCellState Dead

testSurvivalRule = testCase "Survival rule" $
    let liveNeighboursNumber = 2
        prevCellState = Alive
        newCellState = cellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must survive" newCellState Alive

testBecomeAliveRule = testCase "Cell become alive" $
    let liveNeighboursNumber = 3
        prevCellState = Dead
        newCellState = cellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must become alive" newCellState Alive

testSingularDeadGrid = testCase "Singular dead grid" $
    Grid [[Dead]] @=? gridState (Grid [[Dead]])
