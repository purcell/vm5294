module Parse (parseFile) where

import           Control.Monad      (liftM)
import           Text.Parsec
import           Text.Parsec.String
import           Types

source :: Parser Src
source = (string "in" >> return SrcIn)
         <|> (string "null" >> return SrcNull)
         <|> (string "a" >> return SrcA)
         <|> liftM SrcInt vmint
         <|> liftM SrcCPU cpunum

dest :: Parser Dest
dest = (string "out" >> return DestOut)
       <|> (string "a" >> return DestA)
       <|> (string "null" >> return DestNull)
       <|> liftM DestCPU cpunum

nat :: Parser Int
nat = read <$> many1 digit

cpunum :: Parser CPUNum
cpunum = char '#' >> nat <?> "cpu number"

vmint :: Parser VMInt
vmint = do
  symbol <- oneOf "+-"
  n <- nat
  return $ (if symbol == '+' then 1 else -1) * n


instruction :: Parser Instruction
instruction = (string "mov" >> skipMany1 space >> (MOV <$> (source <* skipMany space) <*> (char ',' >> skipMany space *> dest)))
              <|> try (string "swp" >> return SWP)
              <|> try (string "sav" >> return SAV)
              <|> try (string "add" >> skipMany1 space >> (ADD <$> source))
              <|> try (string "sub" >> skipMany1 space >> (SUB <$> source))
              <|> try (string "jmp" >> skipMany1 space >> (JMP <$> vmint))
              <|> try (string "jez" >> skipMany1 space >> (JEZ <$> vmint))
              <|> try (string "jnz" >> skipMany1 space >> (JNZ <$> vmint))
              <|> try (string "jgz" >> skipMany1 space >> (JGZ <$> vmint))
              <|> try (string "jlz" >> skipMany1 space >> (JLZ <$> vmint))
              <?> "instruction"

lineWithOptionalComment :: Parser a -> Parser a
lineWithOptionalComment p = p <* skipMany space <* optional comment <* endOfLine
  where comment = char '~' >> skipMany (noneOf "\r\n")

program :: Parser Program
program = Program <$> lineWithOptionalComment cpunum
                  <*> many (lineWithOptionalComment instruction)


parseFile :: String -> IO (Either ParseError [Program])
parseFile = parseFromFile (many1 program <* eof)
