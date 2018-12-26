list = [1,2,3,4,5]


intFromTo :: Int -> Int -> [Int]
intFromTo a b = a : (intFromTo (a + 1)b)

intFromTo a b
    | a <= b = : (intFromTo(a +1)b)
    | otherwise = []Int
