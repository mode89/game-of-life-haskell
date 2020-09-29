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
    , testSingularGridLiveNeighboursNumber
    , testSlidingWindow
    , testSlidingWindowLongerList
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

testSlidingWindow = testCase "Sliding window" $
    [[0, 1], [1, 2]] @=? slidingWindow 2 [0, 1, 2]

testSlidingWindowLongerList = testCase "Sliding window on longer list" $
    [[0, 1], [1, 2], [2, 3], [3, 4]] @=? slidingWindow 2 [0, 1, 2, 3, 4]
