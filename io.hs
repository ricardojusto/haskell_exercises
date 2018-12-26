-- todos os programas do haskell procuram sempre uma definição do main
 main :: IO()
 main = putStrLn "Hello World"

 --do notation

 do
  line <- getLine
  let newLine = line ++ "!!"
  putStrLn newLine
