module CoreSpec ( tests ) where

import GameOfLife.Core
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Core"
    [ testUnderpopulationRule
    , testOvercrowdingRule
    , testSurvivalRule
    , testBecomeAliveRule
    , testSingularDeadGrid
    , testSingularGridLiveNeighboursNumber
    , testGridPadding
    , testNeighboursInWindow
    , testCountAliveCellsInList
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

testSingularGridLiveNeighboursNumber =
    testCase "Singular grid live neighbours number" $
        [[0]] @=? gridLiveNeighboursNumber (Grid [[Dead]])

testGridPadding = testCase "Grid padding" $
    let input = Grid [[Alive]]
        output = Grid [[Dead,  Dead, Dead],
                       [Dead, Alive, Dead],
                       [Dead,  Dead, Dead]]
    in output @=? paddedGrid input

testNeighboursInWindow = testCase "List neighbours inside a window" $
    let window = [[Dead, Alive,  Dead],
                  [Dead,  Dead, Alive],
                  [Dead, Alive,  Dead]]
        neighbours = [Alive, Alive, Alive, Dead]
    in neighbours @=? neighboursInWindow window

testCountAliveCellsInList = testCase "Count alive cells in a list" $
    2 @=? countAliveCellsInList [Alive, Dead, Alive, Dead]
