list = [1,2,3,4,5]

secondToLast ::[a] -> a
secondToLast (a : []) = a
secondToLast (a : b : []) = a
secondToLast (head : tail) = secondToLast(tail)

-- secondToLast(list)
