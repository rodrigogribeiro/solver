module Main where

import Options.Applicative hiding ((<+>))
import System.FilePath
import Solver.ConstraintSolver
import Parser.ConstraintParser hiding (optional)
import Utils.Pretty (pprint, (<+>), text)
import Solver.SolverMonad (TyCtx (..), VarCtx(..))
import qualified Data.Map as Map    

data Config = Config {
                inputFile  :: FilePath
              , outputFile :: Maybe FilePath                
              } deriving Show

config = Config <$> strOption
                      (  long "input-file"
                      <> short 'i'        
                      <> metavar "INPUT"
                      <> help "Constraint input file")
                <*> optional
                      (strOption
                         (  long "output-file"
                         <> short 'o'        
                         <> metavar "OUTPUT"
                         <> help "Output file"))

opts :: ParserInfo Config
opts = info (config <**> helper)
            ( fullDesc
            <> progDesc "Infer missing typedef's for a constraint in INPUT file"
            <> header "Constraint solver for typedef inference" )                      
                      

outfile :: Maybe FilePath -> FilePath
outfile Nothing = "./result.txt"
outfile (Just f) = f                  
       
execSolver :: Maybe FilePath -> String -> IO ()
execSolver fp s = case parser s of
                    Left err -> putStrLn err
                    Right c  ->
                        do
                          inf <- solver c
                          case inf of
                            Left err'  -> putStrLn err'
                            Right inf' -> do
                                            writeFile (outfile fp)
                                                      (writeInfo inf')

writeInfo :: (TyCtx, VarCtx) -> String
writeInfo (tcx,vcx) = t ++ v
                      where
                        t = Map.foldrWithKey gentydef [] (tyctx tcx)
                        v = Map.foldrWithKey genvardef [] (varctx vcx)    
                        gentydef n t ac = (show $ text "typedef" <+>
                                                 pprint t <+>
                                                 pprint n <+>
                                                 text ";\n") ++ ac
                        genvardef n (t,_) ac = (show $ pprint t <+>
                                                      pprint n <+>
                                                      text ";\n") ++ ac                              
                                                      
main :: IO ()
main = do
        cfg <- execParser opts
        cont <- readFile (inputFile cfg)
        execSolver (outputFile cfg) cont        

        
