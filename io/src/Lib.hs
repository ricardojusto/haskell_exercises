module Lib where

cat :: IO ()
cat = do
    c <- getContents
    putStr c

addNums :: IO ()
addNums = do
    c <- getContents
    let total = sum(map read (lines c))
    putStr (show total)

tac :: IO ()
tac = do
    c <- getContents
    let tac = unlines(reverse (lines c)) -- (unlines . reverse . lines) c
    putStr tac


-- ver slide com o interact sobre como aplicar function composition e o interact
