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

-- 4 TODO

zeroToTen = filter (>= 0) . filter (<=10)

squareRoots = map sqrt . filter (>= 0)

countBetween min max = count (filter(<= max) . filter (>= min ))
    where
        count []        = 0
        count (x:xs)    = 1 + count xs
