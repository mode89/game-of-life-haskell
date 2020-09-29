module SlidingWindowSpec ( tests ) where

import GameOfLife.SlidingWindow
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Sliding window"
    [ testList
    , testLongerList
    , testGrid3x2
    , testGridThroughColumns
    , testGrid2x3
    , testGrid3x3
    , testGrid3x3WindowSize3
    ]

testList = testCase "List" $
    [[0, 1], [1, 2]] @=? slidingWindow 2 [0, 1, 2]

testLongerList = testCase "Longer list" $
    [[0, 1], [1, 2], [2, 3], [3, 4]] @=? slidingWindow 2 [0, 1, 2, 3, 4]

testGrid3x2 = testCase "3x2 grid" $
    [[[1, 2], [3, 4]], [[3, 4], [5, 6]]] @=?
        slidingWindow2d 2 [[1, 2], [3, 4], [5, 6]]

testGridThroughColumns = testCase "Slide through columns only" $
    let input = [[1, 2, 3],
                 [4, 5, 6]]
        output = [[[1, 2],
                  [4, 5]],
                 [[2, 3],
                  [5, 6]]]
    in output @=? slidingWindowThroughColumns 2 input

testGrid2x3 = testCase "2x3 grid" $
    let input = [[1, 2, 3],
                 [4, 5, 6]]
        output = [[[1, 2],
                   [4, 5]],
                  [[2, 3],
                   [5, 6]]]
    in output @=? slidingWindow2d 2 input

testGrid3x3 = testCase "3x3 grid" $
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

testGrid3x3WindowSize3 = testCase "3x3 grid with window size 3" $
        let input = [[1, 2, 3],
                     [4, 5, 6],
                     [7, 8, 9]]
            output = [[[1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9]]]
        in output @=? slidingWindow2d 3 input
