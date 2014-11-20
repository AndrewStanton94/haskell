-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||), (&&), gcd) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2  ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

infixr 3 &&
-- Naive implementation of &&
-- (&&) :: Bool -> Bool -> Bool
-- True  && True   = True
-- True  && False  = False
-- False && True   = False
-- False && False  = False

-- Implementation 2 Edge case && Wildcards
-- (&&) :: Bool -> Bool -> Bool
-- True  && True   = True
-- _     && _      = False

(&&) :: Bool -> Bool -> Bool
False  && _        = False
True   && a        = a

-- infixr 1 !
-- (!) :: Bool -> Bool
-- (!) a = not a

exOr :: Bool -> Bool -> Bool
True  `exOr` False = True
False `exOr` True  = True
_     `exOr` _     = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True  x y = x
ifThenElse False x y = y

daysInMonth :: Int -> Int
daysInMonth 2   = 28
daysInMonth 4   = 30
daysInMonth 5   = 30
daysInMonth 9   = 30
daysInMonth 11  = 30
daysInMonth x   = 31

validDate :: Int -> Int -> Bool
validDate d m = d <= daysInMonth m

-- Recursive
sumNumbers :: Int -> Int
sumNumbers x
    | x <  1        = error "Positive numbers only"
    | x == 1        = 1
    | otherwise     = x + sumNumbers(x - 1)

sumSquares :: Int -> Int
sumSquares x
    | x <  1        = error "Positive numbers only"
    | x == 1        = 1
    | otherwise     = x * x + sumSquares(x - 1)

power :: Int -> Int -> Int
power base exponent
    | exponent == 1 = base
    | otherwise     = base *  power base (exponent - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo min max
    | min >  max    = 0
    | min == max    = min
    | otherwise     = min + sumFromTo (min + 1 ) max

gcd :: Int -> Int -> Int
gcd x y
    | x == y    = x
    | x >  y    = gcd (x-y) y
    | otherwise = gcd (y-x) x

-- intSquareRoot :: Int -> Int

fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m


intSquareRoot :: Int -> Int
intSquareRoot x = bf x x
    where
        bf target try
            | try * try <= target   = try
            |otherwise              = bf target (try-1)
