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


data Token = RightToken | LeftToken | PlusToken | MinusToken | ReadToken | WriteToken | WhileToken | EndToken deriving Eq

tokenize :: String -> [Token]
tokenize []       = []
tokenize ('>':xs) = RightToken : tokenize xs
tokenize ('<':xs) = LeftToken  : tokenize xs
tokenize ('+':xs) = PlusToken  : tokenize xs
tokenize ('-':xs) = MinusToken : tokenize xs
tokenize (',':xs) = ReadToken  : tokenize xs
tokenize ('.':xs) = WriteToken : tokenize xs
tokenize ('[':xs) = WhileToken : tokenize xs
tokenize (']':xs) = EndToken   : tokenize xs
tokenize (_:xs)   = tokenize xs


data Node = ShiftNode Int | AddNode Int | ReadNode | WriteNode | LoopNode [Node]

parse :: [Token] -> [Node]
parse [] = []
parse (ReadToken:ts)  = ReadNode  : parse ts
parse (WriteToken:ts) = WriteNode : parse ts
parse (WhileToken:ts) = LoopNode (parse is) : parse os where
    splitInnerOuter :: [Token] -> ([Token], [Token])
    splitInnerOuter xs = split' xs 0 0 where
        split' (EndToken:ys)   i 0 = splitAt i xs
        split' (EndToken:ys)   i n = split' ys (i + 1) (n - 1)
        split' (WhileToken:ys) i n = split' ys (i + 1) (n + 1)
        split' (_:ys)       i n = split' ys (i + 1) n
    
    (is, os) = splitInnerOuter ts

parse (t:ts)
    | t `elem` [RightToken, LeftToken] = let s = sequence [RightToken, LeftToken] (t:ts) in (ShiftNode $ occurDifference RightToken LeftToken s) : parse (drop (length s) (t:ts))
    | t `elem` [PlusToken, MinusToken] = let s = sequence [PlusToken, MinusToken] (t:ts) in (AddNode   $ occurDifference PlusToken MinusToken s) : parse (drop (length s) (t:ts))
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
    interpret' _ TapeError        = putStrLn "exit: tapeError" >> return TapeError
    interpret' [] t               = return t
    interpret' (ShiftNode n:cs) t = interpret' cs (shiftTape n t)
    interpret' (AddNode   n:cs) t = interpret' cs (alterTape n t)
    interpret' (ReadNode   :cs) t = getChar >>= (\a -> interpret' cs $ setTape (asciiToInt a) t)
    interpret' (WriteNode  :cs) (Tape t p) = (putChar . intToAscii) (t !! p) >> interpret' cs (Tape t p)
    interpret' (LoopNode  c:cs) (Tape t p) = if t !! p > 0 then interpret' c (Tape t p) >>= \tape -> interpret' (LoopNode c:cs) tape else interpret' cs (Tape t p)