module Main where

import           Eval
import           Parse
import           System.Environment (getArgs)
import           Types


runFile :: FilePath -> IO ()
runFile sourceFile = do
  parseResult <- parseFile sourceFile
  case parseResult of
    Left err -> print err
    Right program -> do
      state <- run readInt writeInt program
      putStrLn $ "A: " ++ show (cpuRegA state)
      putStrLn $ "B: " ++ show (cpuRegB state)
  where
    readInt = read <$> getLine
    writeInt = print

main :: IO ()
main = do
  sourceFile:_ <- getArgs
  runFile sourceFile
