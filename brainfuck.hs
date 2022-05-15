module Brainfuck where

interpret :: String -> IO ()
interpret []     = putStrLn "exit"
interpret (x:xs) = undefined