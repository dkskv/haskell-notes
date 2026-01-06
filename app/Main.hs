module Main (main) where

import System.IO (hFlush, stdout)
import Snake (snake)
import ValidPairs (validPairs)

main :: IO ()
main = mainMenu

mainMenu :: IO ()
mainMenu = do
	putStrLn "Choose program to run:"
	putStrLn "1) Snake (interactive spiral)"
	putStrLn "2) ValidPairs (compute and print valid pairs)"
	putStrLn "q) Quit"
	putStr "Enter choice: "
	hFlush stdout
	choice <- getLine
	case choice of
		"1" -> snake >> continue
		"2" -> print validPairs >> continue
		"q" -> putStrLn "Goodbye."
		"Q" -> putStrLn "Goodbye."
		_   -> putStrLn "Invalid choice." >> continue
  where
	continue = putStrLn "" >> mainMenu
