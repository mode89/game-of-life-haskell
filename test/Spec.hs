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
    , testSlidingWindow3x2
    , testSlidingWindowThroughColumns
    , testSlidingWindow2x3
    , testSlidingWindow3x3
    , testSlidingWindow3x3_WindowSize3
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

testSlidingWindow3x2 = testCase "Sliding window on 3x2 grid" $
    [[[1, 2], [3, 4]], [[3, 4], [5, 6]]] @=?
        slidingWindow2d 2 [[1, 2], [3, 4], [5, 6]]

testSlidingWindowThroughColumns = testCase "Sliding window through columns" $
    let input = [[1, 2, 3],
                 [4, 5, 6]]
        output = [[[1, 2],
                  [4, 5]],
                 [[2, 3],
                  [5, 6]]]
    in output @=? slidingWindowThroughColumns 2 input

testSlidingWindow2x3 = testCase "Sliding window on 2x3 grid" $
    let input = [[1, 2, 3],
                 [4, 5, 6]]
        output = [[[1, 2],
                   [4, 5]],
                  [[2, 3],
                   [5, 6]]]
    in output @=? slidingWindow2d 2 input

testSlidingWindow3x3 = testCase "Sliding window on 3x3 grid" $
    let input = [[1, 2, 3],
                 [4, 5, 6],
                 [7, 8, 9]]
        output = [[[1, 2],
                   [4, 5]],
                  [[2, 3],
                   [5, 6]],
                  [[4, 5],
                   [7, 8]],
                  [[5, 6],
                   [8, 9]]]
    in output @=? slidingWindow2d 2 input

testSlidingWindow3x3_WindowSize3 =
    testCase "Sliding window on 3x3 grid with window size 3" $
        let input = [[1, 2, 3],
                     [4, 5, 6],
                     [7, 8, 9]]
            output = [[[1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9]]]
        in output @=? slidingWindow2d 3 input
