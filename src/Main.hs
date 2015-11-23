{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Monad            (when)
import           Control.Monad.State.Lazy (MonadState, State)
import qualified Control.Monad.State.Lazy as St
import           Data.Vector              (Vector)
import qualified Data.Vector              as V


type CPUNum = Int
type VMInt = Int
type InsCount = Int

data Src = SrcIn
         | SrcA
         | SrcNull
         | SrcInt VMInt
         | SrcCPU CPUNum

data Dest = DestOut
          | DestA
          | DestNull
          | DestCPU CPUNum

data Instruction = MOV Src Dest
                 | SWP
                 | SAV
                 | ADD Src
                 | SUB Src
                 | JMP InsCount
                 | JEZ InsCount
                 | JNZ InsCount
                 | JGZ InsCount
                 | JLZ InsCount

data CPUState = CPUState { cpuProgram    :: Vector Instruction
                         , cpuInsPointer :: InsCount
                         , cpuRegA       :: VMInt
                         , cpuRegB       :: VMInt
                         }

newtype VM a = VM { unVM :: State CPUState a }
             deriving (Functor, Applicative, Monad, MonadState CPUState)

run :: [Instruction] -> CPUState
run program = (St.execState . unVM) execute initState
  where initState = CPUState (V.fromList program) 0 0 0

execute :: VM ()
execute = do
  state <- St.get
  when (cpuInsPointer state < V.length (cpuProgram state)) $ do
    if cpuInsPointer state < 0
      then St.put (state { cpuInsPointer = 0 })
      else eval $ cpuProgram state V.! cpuInsPointer state
    execute


receive :: Src -> VM VMInt
receive SrcIn      = undefined
receive SrcA       = St.liftM cpuRegA St.get
receive SrcNull    = return 0
receive (SrcInt n) = return n
receive (SrcCPU n) = undefined

send :: Dest -> VMInt -> VM ()
send DestOut     i = undefined
send DestA       i = St.modify $ \s -> s { cpuRegA = i }
send DestNull    i = return ()
send (DestCPU n) i = undefined


eval :: Instruction -> VM ()
eval (MOV src dest) = evalAndNext $ receive src >>= send dest
eval (SWP         ) = evalAndNext $ St.modify (\s -> s { cpuRegA = cpuRegB s
                                                       , cpuRegB = cpuRegA s })
eval (SAV         ) = evalAndNext $ St.modify (\s -> s { cpuRegB = cpuRegA s })
eval (ADD src     ) = evalAndNext $ do
  val <- receive src
  St.modify (\s -> s { cpuRegA = cpuRegA s + val })
eval (SUB src     ) =  do
  val <- receive src
  St.modify (\s -> s { cpuRegA = cpuRegA s - val })
eval (JMP count   ) = jump count
eval (JEZ count   ) = jumpWhenA (== 0) count
eval (JNZ count   ) = jumpWhenA (/= 0) count
eval (JGZ count   ) = jumpWhenA (> 0) count
eval (JLZ count   ) = jumpWhenA (< 0) count


jump :: InsCount -> VM ()
jump count = St.modify (\s -> s { cpuInsPointer = cpuInsPointer s + count })

jumpWhenA :: (VMInt -> Bool) -> InsCount -> VM ()
jumpWhenA f count = do
  state <- St.get
  jump (if f (cpuRegA state) then count else 1)

evalAndNext :: VM () -> VM ()
evalAndNext action = action >> jump 1



main :: IO ()
main = do
  let program = [ MOV (SrcInt 1) DestA
                , SWP
                , MOV (SrcInt 2) DestA
                , ADD (SrcInt 3)
                ]
  let state = run program
  putStrLn $ "A: " ++ show (cpuRegA state)
  putStrLn $ "B: " ++ show (cpuRegB state)
