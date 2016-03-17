import Test.Tasty
import Test.Tasty.HUnit

import Solver.ConstraintSolver
import Data.Constraints
import Data.Type
import Data.BuiltIn    
import Parser.ConstraintParser hiding (typeDefParser)
import Solver.SolverMonad    
import Utils.Pretty hiding (semi)
import Data.Either
import Control.Monad
import qualified Data.Map as Map    

main :: IO ()
main = defaultMain tests


cases :: String
cases = "./test/cases/"         
       
tests :: TestTree
tests = testGroup "Unit tests"
                  [
                    doTest "a.c"
                  , doTest "c.c"
                  , doTest "s.c"
                  , doTest "e.c"
                  , doTest "f.c"
                  , doTest "TA.c"
                  , doTest "TB.c"
                  , doTest "T2.c"
                  , doTest "T3.c"
                  , doTest "T4.c"
                  , doTest "T8.c"  
                  , doTest "T10.c"
                  , doTest "T11.c"
                  , doTest "paperExample.c"  
                  , doTest "testPrintf.c"
                  , doTest "floatMatrix.c"  
                  , doTest "paperExampleTweetnacl.c"
                  , doTest "shift.c"
                  , doTest "T12.c"
                  , doTest "T13.c"
                  ]


doTest s
    = testCase ("Testing " ++ s) $
      do
        ctr <- readFile (cases ++ s ++ ".ctr")
        res <- readFile (cases ++ s ++ ".out")
        case parser ctr of
          Left err -> error $ "Error on constraint parser:\n" ++ err
          Right c  -> case parse resParser "" res of
                         Left err' -> error $ "Error on answer parser:\n" ++ show err'
                         Right (tx,vx) ->
                             do 
                               r <- solver c
                               case r of
                                  Left err'' -> error $ "Error on constraint solver:\n" ++ show err''
                                  Right (tx', vx') ->
                                      unless (tx == tx' && vx == vx')
                                             (error $ printResults tx tx' vx vx')

printResults tx tx' vx vx'
       = show $ text "Solver infered types:\n" <+>
                pprint tx' <+>
                text "\nExpected types:\n" <+>
                pprint tx         <+>
                text "\nSolver infered variables:\n" <+>
                pprint vx' <+>
                text "\nExpected variables:\n" <+>
                pprint vx         
                                          

resParser :: Parser (TyCtx, VarCtx)
resParser = f <$> many (typeDefParser <|> varDefParser)
            where
              f = uncurry g . partitionEithers
              g a b = (TyCtx (Map.fromList a)
                      , VarCtx (Map.fromList b))       
                      
typeDefParser :: Parser (Either (Name,Ty) (Name, (Ty,Bool)))
typeDefParser  = f <$> reserved "typedef" <*>
                       typeParser <*>
                       nameParser <*> semi
                   where
                     f _ t n _ = Left (n,t)

varDefParser :: Parser (Either (Name,Ty) (Name, (Ty,Bool)))
varDefParser  = f <$> typeParser <*> nameParser <*> semi
                  where
                    f t n _ = Right (n,(t,False))
