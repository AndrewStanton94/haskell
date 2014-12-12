{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []


headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne(x : xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead (x : xs) = x:x:xs
duplicateHead [] = []

rotate :: [a] -> [a]
rotate (x: x2: xs) = x2:x:xs
rotate a = a

listLength :: [a] -> Int
listLength[]	= 0
listLength(x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll[]       = 1
multAll(x:xs)   = x * multAll xs

andAll :: [Bool] -> Bool
andAll []       = True
andAll(x: xs)   = x && andAll xs

countElems :: Int -> [Int] -> Int
countElems i [] = 0
countElems i (x: xs)   
    | i == x = 1 + countElems i xs
    | i /= x = 0 + countElems i xs

removeAll :: Int -> [Int] -> [Int]
removeAll i []  = []
removeAll i (x: xs)
    | i == x = [] ++ removeAll i xs
    | i /= x = [x]++ removeAll i xs

type StudentMark = (String, Int)
listMarks :: String -> [StudentMark] -> [Int]
listMarks name []           = []
listMarks name studentMarks = [mark | (nameMark, mark) <- studentMarks, nameMark == name]
                            --[return | stepperValues <- collection, filter]

prefix :: [Int] -> [Int] -> Bool
prefix (x: xs) (y: ys)
    | x == y    = True && prefix xs ys
    | otherwise = False
prefix _ _    = True

subSequence :: [Int] -> [Int] -> Bool
subSequence (x: xs) (y: ys)
    | prefix (x: xs) (y: ys) == False    = subSequence xs ys
    | otherwise = True 
subSequence _ _ = True