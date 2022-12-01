module Main where


import qualified Day1 (solve)

solutions :: [IO ()]
solutions = [Day1.solve]


main :: IO ()
main = do
    putStrLn "Show which day's answers?"
    day <- readLn
    solutions !! (day - 1)
