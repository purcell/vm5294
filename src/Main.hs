module Main where

import           Eval
import           Types


main :: IO ()
main = do
  let program = [ MOV (SrcInt 1) DestA
                , SWP
                , MOV (SrcInt 2) DestA
                , ADD (SrcInt 3)
                , MOV SrcIn DestA
                , MOV SrcA DestOut
                ]
  state <- run readInt writeInt program
  putStrLn $ "A: " ++ show (cpuRegA state)
  putStrLn $ "B: " ++ show (cpuRegB state)

  where
    readInt = read <$> getLine
    writeInt = print
