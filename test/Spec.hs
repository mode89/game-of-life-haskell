import Test.Tasty
import Test.Tasty.HUnit
import qualified CoreSpec
import qualified SlidingWindowSpec

main = defaultMain tests

tests = testGroup "Tests"
    [ CoreSpec.tests
    , SlidingWindowSpec.tests
    ]
