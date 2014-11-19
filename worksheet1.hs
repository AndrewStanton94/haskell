timesTen :: Int -> Int
timesTen x = 10 * x

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r d = d * areaOfCircle r

dSqr :: Float -> Float -> Float
dSqr p1 p2 = (p1 - p2) ^ 2

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ( dSqr y1 y2 + dSqr x1 x2 )

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

divisibleBy :: Int -> Int -> Bool
divisibleBy d b = (d `mod` b) == 0
-- backticks: function to operator
-- brackets : operator to function

isEven :: Int -> Bool
isEven i = divisibleBy i 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral ( sumThree a b c) / 3

absolute :: Int -> Int 
absolute x = if x >= 0 then x else -x
-- Brackets around the parameter
