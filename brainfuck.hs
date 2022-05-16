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


data Token = OpRight | OpLeft | OpPlus | OpMinus | OpRead | OpWrite | OpWhile | OpEnd deriving (Show, Eq)

tokenize :: String -> [Token]
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


data Node = Shift Int | Add Int | Read | Write | Loop [Node] deriving Show

parse :: [Token] -> [Node]
parse [] = []
parse (t:ts)
    | t `elem` [OpRight, OpLeft] = let s = sequence [OpRight, OpLeft] (t:ts) in (Shift $ occurDifference OpRight OpLeft s) : parse (drop (length s) (t:ts))
    | t `elem` [OpPlus, OpMinus] = let s = sequence [OpPlus, OpMinus] (t:ts) in (Add   $ occurDifference OpPlus OpMinus s) : parse (drop (length s) (t:ts))
    | t == OpRead                = Read  : parse ts
    | t == OpWrite               = Write : parse ts
    | t == OpWhile               = let (is, os) = splitInnerOuter ts in Loop (parse is) : parse os
    | otherwise = parse ts
    where
        sequence :: Eq a => [a] -> [a] -> [a]
        sequence _   (x:[]) = [x]
        sequence ops (x:y:xs)
            | not (x `elem` ops)                 = []
            | x `elem` ops && not (y `elem` ops) = [x]
            | x `elem` ops && y `elem` ops       = x : y : sequence ops xs
        
        occurDifference :: Eq a => a -> a -> [a] -> Int
        occurDifference x y xs = (length . filter (x ==)) xs - (length . filter (y ==)) xs

        splitInnerOuter :: [Token] -> ([Token], [Token])
        splitInnerOuter xs = split' xs 0 0 where
            split' (OpEnd:ys)   i 0 = splitAt i xs
            split' (OpEnd:ys)   i n = split' ys (i + 1) (n - 1)
            split' (OpWhile:ys) i n = split' ys (i + 1) (n + 1)
            split' (_:ys)       i n = split' ys (i + 1) n

interpret :: String -> IO ()
interpret code = interpret' (parse $ tokenize code) (Tape [0] 0) where
    interpret' :: [Node] -> Tape -> IO ()
    interpret' _ TapeError       = putStrLn "exit: tapeError"
    interpret' [] _              = putStrLn "exit"
    interpret' (c:cs) (Tape t p) = undefined