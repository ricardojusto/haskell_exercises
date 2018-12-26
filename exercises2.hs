head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

take' :: Int -> [a] -> [a]
take' 0 l = []
take' _ [] = []
take' n (h :t) = h : (take (n -1) t)

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' n (h:t) = drop (n -1) t

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (h:t) = e == h || elem e t

concat' :: [a] -> [a] -> [a]
concat' l [] = l
concat' [] l = l
concat' (h:t) l = h: (concat' t l)

splitAt :: Int -> [a] ->> ([a], [a])
splitAt 0 xs = ([], xs)
splitAt n (h:t) =
  let (before, after) = splitAt (n -1) t
  in (h : before, after)
