module Eval (run)
       where

import           Control.Monad            (when)
import qualified Control.Monad.State.Lazy as St
import qualified Data.Vector              as V
import           Types


run :: IO VMInt -> (VMInt -> IO ()) -> [Instruction] -> IO CPUState
run readInt writeInt program = St.execStateT execute initState
  where initState = CPUState (V.fromList program) 0 0 0 (St.lift readInt) (St.lift . writeInt)

execute :: VM ()
execute = do
  state <- St.get
  let pos = cpuInsPointer state
  when (pos < V.length (cpuProgram state)) $ do
    if pos < 0
      then St.put (state { cpuInsPointer = 0 })
      else eval $ cpuProgram state V.! pos
    execute


receive :: Src -> VM VMInt
receive SrcIn      = cpuReadInt =<< St.get
receive SrcA       = St.liftM cpuRegA St.get
receive SrcNull    = return 0
receive (SrcInt n) = return n
receive (SrcCPU n) = undefined

send :: Dest -> VMInt -> VM ()
send DestOut     i = flip cpuWriteInt i =<< St.get
send DestA       i = St.modify $ \s -> s { cpuRegA = i }
send DestNull    i = return ()
send (DestCPU n) i = undefined


eval :: Instruction -> VM ()
eval (MOV src dest) = evalAndNext $ receive src >>= send dest
eval SWP            = evalAndNext $ St.modify (\s -> s { cpuRegA = cpuRegB s
                                                       , cpuRegB = cpuRegA s })
eval SAV            = evalAndNext $ St.modify (\s -> s { cpuRegB = cpuRegA s })
eval (ADD src)      = evalAndNext $ do
  val <- receive src
  St.modify (\s -> s { cpuRegA = cpuRegA s + val })
eval (SUB src)      =  do
  val <- receive src
  St.modify (\s -> s { cpuRegA = cpuRegA s - val })
eval (JMP count)    = jump count
eval (JEZ count)    = jumpWhenA (== 0) count
eval (JNZ count)    = jumpWhenA (/= 0) count
eval (JGZ count)    = jumpWhenA (> 0) count
eval (JLZ count)    = jumpWhenA (< 0) count


jump :: InsCount -> VM ()
jump count = St.modify (\s -> s { cpuInsPointer = cpuInsPointer s + count })

jumpWhenA :: (VMInt -> Bool) -> InsCount -> VM ()
jumpWhenA f count = do
  state <- St.get
  jump (if f (cpuRegA state) then count else 1)

evalAndNext :: VM () -> VM ()
evalAndNext action = action >> jump 1
