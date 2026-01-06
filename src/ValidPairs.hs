module ValidPairs (validPairs) where

import Data.List (group, sort)

-- Решение для https://thecode.media/small-talk/.
validPairs :: [Pair Int]
validPairs = do
    -- 1. Вычисление пар слагаемых для каждой суммы.
    let groupedPairs = termPairs <$> validSums
    -- 2. Нахождение уникальных произведений пар (присутствующих единожды среди всех сумм).
    let uniqProducts = singles $ product <$> (groupedPairs >>= id)
    -- 3. Удаление пар, дающих неуникальные(среди всех сумм) произведения.
    x <- filter ((`elem` uniqProducts) . product) <$> groupedPairs
    -- 4. Выбор пар, оставшихся единственными в своей группе(сумме).
    clearNonSingle x

singles :: Ord a => [a] -> [a]
singles = (>>= clearNonSingle) . group . sort

clearNonSingle :: [a] -> [a]
clearNonSingle (y:[]) = [y]
clearNonSingle _ = []

-- Результат разложения: пара слагаемых или множителей.
data Pair a = Pair a a deriving Show

-- Для применения к Pair методов списка.
instance Foldable Pair where
    foldr f ini (Pair a b) = foldr f ini [a,b]

-- Разложения n на 2 натуральных слагаемых > 1
termPairs :: Int -> [Pair Int]
termPairs n = [Pair x y | x <- [2..n], y <- [2..n], x + y == n, x <= y]

-- Список всех валидных сумм (11,17,23...)
validSums :: [Int]
validSums = filter isSumValid [1..99]

-- Правило: сумма должна быть нечетной.
-- Правило: во всех разложениях суммы должно присутствовать составное число.
isSumValid :: Int -> Bool
isSumValid n = odd n && (not . null) pairs && all (any isComposite) pairs  where
    pairs = termPairs n

isComposite :: Int -> Bool
isComposite n = (n /= 1) && (not . isPrime $ n)

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

factors :: Int -> [Int]
factors m = [x | x <- [1..m], mod m x == 0]