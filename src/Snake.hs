module Snake (snake) where

import Control.Monad.State
import Data.Matrix

-- Вывести в терминал квадрат, заполненный числами от 1 до n^2 по спирали к центру

snake :: IO ()
snake = do
    putStrLn "Input n:"
    input <- getLine
    let n = (read input :: Int)
    let points = runSnakePoints n
    putStr . show $ fillMatrix (zero n n) (zip points [1..])

fillMatrix = foldr $ \((Point x y), i) -> setElem i (y, x)

data Point = Point Int Int deriving Show
data Env = Env { 
    countOfStepsStraight :: Int,
    stepSign:: Int,
    currentPoint :: Point
}
type Snake = [Point]

runSnakePoints n = evalState snakePoints (Env n 1 (Point (0) 1))

snakePoints :: State Env Snake
snakePoints = do 
   n <- gets countOfStepsStraight
   points1 <- replicateM n (step moveX)
   modify $ \e -> e { countOfStepsStraight = pred n }

   n <- gets countOfStepsStraight
   points2 <- replicateM n (step moveY)
   modify $ \e -> e { stepSign = negate (stepSign e) }

   nextPoints <- if n <= 0 then return [] else snakePoints
   return $ points1 ++ points2 ++ nextPoints

step :: (Point -> Int -> Point) -> State Env Point
step move = do
   modify (\e@(Env _ s p) -> e { currentPoint = move p s })
   gets currentPoint

moveX (Point x y) s = Point (x + s) y
moveY (Point x y) s = Point x (y + s)   