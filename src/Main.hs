module Main where

import           Eval
import           Parse
import           System.Environment (getArgs)


runFile :: FilePath -> IO ()
runFile sourceFile = do
  parseResult <- parseFile sourceFile
  case parseResult of
    Left err -> print err
    Right program -> run readInt writeInt program
  where
    readInt = read <$> getLine
    writeInt = print

main :: IO ()
main = do
  sourceFile:_ <- getArgs
  runFile sourceFile
