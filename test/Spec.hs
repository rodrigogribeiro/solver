import Test.Tasty
import Test.Tasty.HUnit

import Solver.ConstraintSolver
import Data.Constraints
import Data.Type
import Data.BuiltIn    
import Parser.ConstraintParser    
import Utils.Pretty    

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Unit tests"
                  [
                    testCase "test" $ doTest "./test/cases/Test3.ctr"
                  ]


doTest s = do
                c <- readFile s
                either print
                       testSolver
                       (parser c)
        
