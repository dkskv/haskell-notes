module Main (main) where

import ValidPairs (validPairs)

main :: IO ()
main = putStr . show $ validPairs
