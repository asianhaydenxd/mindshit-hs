module Brainfuck where

data Tape = Tape [Int] Int | TapeError

newTape :: Tape
newTape = Tape [0] 0

shiftTape :: Int -> Tape -> Tape
shiftTape n (Tape t p)
    | p + n < 0         = TapeError
    | p + n >= length t = Tape (t ++ (replicate (p + n + 1 - length t) 0)) (p + n)
    | otherwise         = Tape t (p + n)

alterTape :: Int -> Tape -> Tape
alterTape n (Tape t p) = Tape (replace t p ((item + n) `mod` 256)) p where
    item = t !! p
    replace l i n
        | i == 0    = n : tail l
        | otherwise = let (x,_:xs) = splitAt i l in x ++ n : xs

setTape :: Int -> Tape -> Tape
setTape n (Tape t p) = alterTape n $ alterTape (-(t !! p)) (Tape t p)


data Token = OpRight | OpLeft | OpPlus | OpMinus | OpRead | OpWrite | OpWhile | OpEnd deriving Eq

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


data Node = Shift Int | Add Int | Read | Write | Loop [Node]

parse :: [Token] -> [Node]
parse [] = []
parse (OpRead:ts)  = Read  : parse ts
parse (OpWrite:ts) = Write : parse ts
parse (OpWhile:ts) = Loop (parse is) : parse os where
    splitInnerOuter :: [Token] -> ([Token], [Token])
    splitInnerOuter xs = split' xs 0 0 where
        split' (OpEnd:ys)   i 0 = splitAt i xs
        split' (OpEnd:ys)   i n = split' ys (i + 1) (n - 1)
        split' (OpWhile:ys) i n = split' ys (i + 1) (n + 1)
        split' (_:ys)       i n = split' ys (i + 1) n
    
    (is, os) = splitInnerOuter ts

parse (t:ts)
    | t `elem` [OpRight, OpLeft] = let s = sequence [OpRight, OpLeft] (t:ts) in (Shift $ occurDifference OpRight OpLeft s) : parse (drop (length s) (t:ts))
    | t `elem` [OpPlus, OpMinus] = let s = sequence [OpPlus, OpMinus] (t:ts) in (Add   $ occurDifference OpPlus OpMinus s) : parse (drop (length s) (t:ts))
    | otherwise                  = parse ts
    where
        sequence :: Eq a => [a] -> [a] -> [a]
        sequence _   []     = []
        sequence ops (x:[])
            | x `elem` ops = [x]
            | otherwise    = []
        sequence ops (x:y:xs)
            | x `elem` ops && y `elem` ops = x : y : sequence ops xs
            | x `elem` ops                 = [x]
            | otherwise                    = []
        
        occurDifference :: Eq a => a -> a -> [a] -> Int
        occurDifference x y xs = (length . filter (x ==)) xs - (length . filter (y ==)) xs


intToAscii :: Int -> Char
intToAscii = toEnum

asciiToInt :: Char -> Int
asciiToInt = fromEnum

interpret :: String -> IO (Tape)
interpret code = interpret' (parse $ tokenize code) (Tape [0] 0) where
    interpret' :: [Node] -> Tape -> IO (Tape)
    interpret' _ TapeError    = putStrLn "exit: tapeError" >> return TapeError
    interpret' [] t           = return t
    interpret' (Shift n:cs) t = interpret' cs (shiftTape n t)
    interpret' (Add   n:cs) t = interpret' cs (alterTape n t)
    interpret' (Read   :cs) t = getChar >>= (\a -> interpret' cs $ setTape (asciiToInt a) t)
    interpret' (Write  :cs) (Tape t p) = (putChar . intToAscii) (t !! p) >> interpret' cs (Tape t p)
    interpret' (Loop  c:cs) (Tape t p) = if t !! p > 0 then interpret' c (Tape t p) >>= \tape -> interpret' (Loop c:cs) tape else interpret' cs (Tape t p)