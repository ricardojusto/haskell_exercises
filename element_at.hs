list = [1,2,3]

elementAt :: [a] -> Int -> a
elementAt [x] b = x
elementAt (h : t) num
    | num == 0  = h
    | otherwise = elementAt t (num -1)
