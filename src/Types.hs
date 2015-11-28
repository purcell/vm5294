module Types where

import           Control.Monad.State.Lazy (StateT)
import           Data.Vector              (Vector)

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
                         , cpuReadInt    :: VM VMInt
                         , cpuWriteInt   :: VMInt -> VM ()
                         }

type VM a = StateT CPUState IO a
