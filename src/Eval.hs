{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (run)
       where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy (MonadState, StateT)
import qualified Control.Monad.State.Lazy as St
import qualified Data.Map                 as M
import           Data.Vector              (Vector)
import qualified Data.Vector              as V
import           Types

data CPUState = CPUState { cpuNum          :: CPUNum
                         , cpuInstructions :: Vector Instruction
                         , cpuInsPointer   :: InsCount
                         , cpuRegA         :: VMInt
                         , cpuRegB         :: VMInt
                         , cpuReadInt      :: VM VMInt
                         , cpuWriteInt     :: VMInt -> VM ()
                         , cpuAsk          :: CPUNum -> VM VMInt
                         , cpuTell         :: CPUNum -> VMInt -> VM ()
                         }

newtype VM a = VM { runVM :: StateT CPUState IO a }
               deriving (Functor, Applicative, Monad, MonadIO, MonadState CPUState)

run :: IO VMInt -> (VMInt -> IO ()) -> [Program] -> IO ()
run readInt writeInt programs = do
  channels <- makeChannels programs
  void $ mapConcurrently (runCPU channels) programs
  where
    runCPU channels program = St.evalStateT (runVM execute) initState
      where
        initState = CPUState mynum (V.fromList $ progInstructions program) 0 0 0 (liftIO readInt) (liftIO . writeInt) (ask channels mynum) (tell channels mynum)
        mynum = progCPU program
    ask channels me n = liftIO $ takeMVar (channels M.! (n, me))
    tell channels me n i = liftIO $ putMVar (channels M.! (me, n)) i


type Channels = M.Map (CPUNum, CPUNum) (MVar VMInt)

makeChannels :: [Program] -> IO Channels
makeChannels programs = M.fromList <$> forM pairs (\p -> (,) p <$> newEmptyMVar)
  where cpuNums = map progCPU programs
        pairs = [(a, b) | a <- cpuNums, b <- cpuNums, a /= b]


execute :: VM ()
execute = do
  state <- St.get
  let pos = cpuInsPointer state
  when (pos < V.length (cpuInstructions state)) $ do
    if pos < 0
      then St.put (state { cpuInsPointer = 0 })
      else do
        let instr = cpuInstructions state V.! pos
        -- liftIO $ putStrLn ("#" ++ show (cpuNum state) ++ " -- " ++ show instr)
        eval instr
    execute


receive :: Src -> VM VMInt
receive SrcIn      = cpuReadInt =<< St.get
receive SrcA       = St.liftM cpuRegA St.get
receive SrcNull    = return 0
receive (SrcInt n) = return n
receive (SrcCPU n) = flip cpuAsk n =<< St.get

send :: Dest -> VMInt -> VM ()
send DestOut     i = flip cpuWriteInt i =<< St.get
send DestA       i = St.modify $ \s -> s { cpuRegA = i }
send DestNull    _ = return ()
send (DestCPU n) i = do
  s <- St.get
  let tell = cpuTell s
  tell n i


eval :: Instruction -> VM ()
eval (MOV src dest) = evalAndNext $ receive src >>= send dest
eval SWP            = evalAndNext $ St.modify (\s -> s { cpuRegA = cpuRegB s
                                                       , cpuRegB = cpuRegA s })
eval SAV            = evalAndNext $ St.modify (\s -> s { cpuRegB = cpuRegA s })
eval (ADD src)      = evalAndNext $ do
  val <- receive src
  St.modify (\s -> s { cpuRegA = cpuRegA s + val })
eval (SUB src)      =  evalAndNext $ do
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
