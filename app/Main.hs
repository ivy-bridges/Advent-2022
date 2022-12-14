module Main where


import qualified Day1  (solve)
import qualified Day2  (solve)
import qualified Day3  (solve)
import qualified Day4  (solve)
import qualified Day5  (solve)
import qualified Day6  (solve)
import qualified Day8  (solve)
import qualified Day9  (solve)
import qualified Day10 (solve)

solutions :: [IO ()]
solutions = [Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, return (), Day8.solve, Day9.solve, Day10.solve]


main :: IO ()
main = do
    putStrLn "Show which day's answers?"
    day <- readLn
    solutions !! (day - 1)
