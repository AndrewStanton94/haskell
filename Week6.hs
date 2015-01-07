{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit

keepPositive = filter (>0)
keepDigits = filter isDigit

addUp = foldr (+) 0 
myConcat = foldr (++) []

mult10 = map (*10)

onlyLowerCase = filter isLower

orAll = foldr (||) False

sumSquares xs = foldr (+) 0 [x*x | x <- xs]

zeroToTen = filter (>= 0) . filter (<=10)

squareRoots = map sqrt . filter (>= 0)

--  7 TODO
countBetween min max [] = 0
countBetween min max (x:xs)
    | min <= x && x <= max  = 1 + countBetween min max xs
    | otherwise             = 0 + countBetween min max xs

--  8 TODO

productSquareRoots list = foldr (*) 1 (squareRoots list)
    -- 1 used in first iteration
