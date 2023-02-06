module Main (main) where

import Snake
import ValidPairs

main :: IO ()
main = putStr . show $ validPairs
