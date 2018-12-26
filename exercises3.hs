-- funções de ordem superior
-- é uma função que recebe outra função como parametro

dobros :: (Num a) => [a] -> [a]
dobros [] = []
dobros (h:t) = (2 * h) : dobros t

triplos :: (Num a) => [a] -> [a]
triplos [] = []
triplos (h:t) = (2 * h) : triplos t

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (h:t) = f h : (map f t)

odds :: (Integral a) => [a] -> [a]
odds [] = []
odds (x:xs) = if (x `mod` 2) /= 0   then x : odds xs
                                    else     odds xs

evens :: (Integral a) => [a] -> [a]
evens [] = []
evens (x:xs) = if (x `mod` 2) == 0   then x : evens xs
                                     else     evens xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x : filter' p xs
                         else     filter' p xs
