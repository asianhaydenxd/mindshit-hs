module Brainfuck where

data Tape = Tape [Int] Int | TapeError deriving Show

newTape :: Tape = Tape [0] 0

shiftTape :: Int -> Tape -> Tape
shiftTape n (Tape t p)
    | p + n >= length t || p + n < 0 = TapeError
    | otherwise                      = Tape t ((p + n) `mod` length t)

alterTape :: Int -> Tape -> Tape
alterTape n (Tape t p) = Tape (replace t p ((item + n) `mod` 256)) p where
    item = t !! p
    replace l i n
        | i == 0    = n : tail l
        | otherwise = let (x,_:xs) = splitAt i l in x ++ n : l

shiftRight :: Tape -> Tape = shiftTape 1

shiftLeft :: Tape -> Tape = shiftTape (-1)

increment :: Tape -> Tape = alterTape 1

decrement :: Tape -> Tape = alterTape (-1)


data Operator = OpRight | OpLeft | OpPlus | OpMinus | OpRead | OpWrite | OpWhile | OpEnd deriving Show

type Code = [Operator]

tokenize :: String -> Code
tokenize []       = []
tokenize ('>':xs) = OpRight : tokenize xs
tokenize ('<':xs) = OpLeft  : tokenize xs
tokenize ('+':xs) = OpPlus  : tokenize xs
tokenize ('-':xs) = OpMinus : tokenize xs
tokenize (',':xs) = OpRead  : tokenize xs
tokenize ('.':xs) = OpWrite : tokenize xs
tokenize ('[':xs) = OpWhile : tokenize xs
tokenize (']':xs) = OpEnd   : tokenize xs
tokenize (_:xs)   = tokenize xs

interpret :: String -> IO ()
interpret code = interpret' (tokenize code) (Tape [0] 0) where
    interpret' :: Code -> Tape -> IO ()
    interpret' _ TapeError       = putStrLn "exit: tapeError"
    interpret' [] _              = putStrLn "exit"
    interpret' (c:cs) (Tape t p) = undefined