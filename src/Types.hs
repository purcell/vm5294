module Types where

type CPUNum = Int
type VMInt = Int
type InsCount = Int

data Src = SrcIn
         | SrcA
         | SrcNull
         | SrcInt VMInt
         | SrcCPU CPUNum
         deriving Show

data Dest = DestOut
          | DestA
          | DestNull
          | DestCPU CPUNum
          deriving Show

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
                 deriving Show

data Program = Program { progCPU          :: CPUNum
                       , progInstructions :: [Instruction]
                       }
