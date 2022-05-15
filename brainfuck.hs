module Brainfuck where

data Tape = Tape [Int] Int

interpret :: String -> IO ()
interpret code = interpret' code (Tape [0] 0) where
    interpret' :: String -> Tape -> IO ()
    interpret' [] _              = putStrLn "exit"
    interpret' (c:cs) (Tape t p) = undefined