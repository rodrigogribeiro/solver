module Main where

import Options.Applicative
import System.FilePath
import Solver.ConstraintSolver
import Parser.ConstraintParser    

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
                          writeFile (outfile fp)
                                    (writeInfo inf)
                                         
main :: IO ()
main = do
        cfg <- execParser opts
        cont <- readFile (inputFile cfg)
        execSolver (outputFile cfg) cont        

        
