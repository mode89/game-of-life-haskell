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
    , testCountAliveNeighboursOnGrid
    , testEvolve
    , testStringFromGrid
    , testEmbedGrid
    , testCellCoordFromGrid
    ]

testUnderpopulationRule = testCase "Underpopulation rule" $
    let liveNeighboursNumber = 0
        prevCellState = Alive
        newCellState = nextCellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must die" newCellState Dead

testOvercrowdingRule = testCase "Overcrowding rule" $
    let liveNeighboursNumber = 4
        prevCellState = Alive
        newCellState = nextCellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must die" newCellState Dead

testSurvivalRule = testCase "Survival rule" $
    let liveNeighboursNumber = 2
        prevCellState = Alive
        newCellState = nextCellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must survive" newCellState Alive

testBecomeAliveRule = testCase "Cell become alive" $
    let liveNeighboursNumber = 3
        prevCellState = Dead
        newCellState = nextCellState prevCellState liveNeighboursNumber
    in assertEqual "Cell must become alive" newCellState Alive

testSingularDeadGrid = testCase "Singular dead grid" $
    Grid [[Dead]] @=? nextGridState (Grid [[Dead]])

testSingularGridLiveNeighboursNumber =
    testCase "Singular grid live neighbours number" $
        [[0]] @=? countAliveNeighboursOnGrid (Grid [[Dead]])

testGridPadding = testCase "Grid padding" $
    let input = Grid [[Alive]]
        output = Grid [[Dead,  Dead, Dead],
                       [Dead, Alive, Dead],
                       [Dead,  Dead, Dead]]
    in output @=? paddedGrid input

testNeighboursInWindow = testCase "List neighbours inside a window" $
    let window = [[Dead, Alive,  Dead],
                  [Dead,  Dead, Alive],
                  [Dead, Alive, Alive]]
        neighbours = [Dead, Alive, Dead, Dead, Alive, Dead, Alive, Alive]
    in neighbours @=? neighboursInWindow window

testCountAliveCellsInList = testCase "Count alive cells in a list" $
    2 @=? countAliveCellsInList [Alive, Dead, Alive, Dead]

testCountAliveNeighboursOnGrid =
    testCase "Count alive neighbours of each cell" $
        [[1, 0]] @=? countAliveNeighboursOnGrid (Grid [[Dead, Alive]])

testEvolve = testCase "Evolve grid" $
    let input = Grid [[Alive, Alive, Alive],
                      [Alive, Alive, Alive],
                      [ Dead,  Dead,  Dead]]
        output = Grid [[Alive,  Dead, Alive],
                       [Alive,  Dead, Alive],
                       [ Dead, Alive,  Dead]]
    in output @=? nextGridState input

testStringFromGrid = testCase "Convert Grid to a string" $
    let input = Grid [[Alive,  Dead],
                      [Alive, Alive]]
        output = "*.\n**"
    in output @=? stringFromGrid input

testEmbedGrid = testCase "Embed one grid into another" $
    let baseGrid = emptyGrid 3 3
        grid = Grid [[ Alive,  Dead ],
                     [ Alive, Alive ]]
        output = Grid [[ Dead,  Dead,  Dead ],
                       [ Dead, Alive,  Dead ],
                       [ Dead, Alive, Alive ]]
    in output @=? embedGrid baseGrid 1 1 grid

testCellCoordFromGrid = testCase "Get coordinates of alive cells" $
    let grid = Grid [[ Alive, Dead ],
                     [ Alive, Alive ]]
        output = [CellCoord 0 0, CellCoord 1 0, CellCoord 1 1]
    in output @=? cellCoordFromGrid grid
