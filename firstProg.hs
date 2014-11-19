mult2 :: Float -> Float
mult2 x = 2 *x

mult4 :: Float -> Float
mult4 x = mult2(mult2 x)

half :: Float -> Float
half x = x / 2

halfInt :: Float -> Int
halfInt x = fromIntegral(x) / 2
