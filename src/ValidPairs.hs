module ValidPairs where

-- Список решений для https://thecode.media/small-talk/.
validPairs = 
    -- 5. Оставляем варианты длинной 1 (где 1 сумме соответствует 1 пара валидных слагаемых).
    -- Повторно уменьшаем вложенность.
    clearNonSingle =<< ( 
    -- 4. Оставляем варианты длинной 1 (где 1 паре слагаемых соответствует 1 пара валидных множителей).
    -- Заодно уменьшаем вложенность: [[xs,xs], [xs], [xs], []] -> [xs, xs].
    -- Тем самым получаем список валидных слагаемых...Запутался)))
    (clearNonSingle =<<) <$> 
    -- 3. Отображаем каждое произведение на валидные множители (дающие валидную сумму).
    (validFactors <$>) <$> 
    -- 2. Отображаем каждую пару слагаемых на произведение.
    (product <$>) <$> 
    -- 1. Отображаем каждую валидную сумму на пары слагаемых: [x, x] -> [[Pair, Pair], [Pair]].
    (termPairs <$> validSums)) 

clearNonSingle (y:[]) = [y]
clearNonSingle _ = []

-- Правило: сумма должна быть нечетной.
-- Правило: во всех разложениях суммы должно присутствовать составное число.
isSumValid n = odd n && all (any isComposite) pairs && (not . null) pairs where
    pairs = termPairs n

-- Список всех валидных сумм (11,17,23...)
validSums = filter isSumValid [2..99]

-- Получить все валидные (дающие валидную сумму) разложения на множители
validFactors n = filter (isSumValid . sum) (factorPairs n)

-- Результат разложения: пара слагаемых или множителей.
-- (не используется список, чтобы избежать большой вложенности списков в промежуточных решениях).
data Pair a = Pair a a deriving Show

-- Для применения к Pair методов списка.
instance Foldable Pair where
    foldr f ini (Pair a b) = foldr f ini [a,b]

-- Разложения n на 2 натуральных множителя > 1
factorPairs n = [Pair x y | x <- [2..n], y <- [2..n], x * y == n, x <= y]
-- Разложения n на 2 натуральных слагаемых > 1
termPairs n = [Pair x y | x <- [2..n], y <- [2..n], x + y == n, x <= y]

isComposite n = (n /= 1) && (not . isPrime $ n)

isPrime n = factors n == [1,n] where
    factors n = [x | x <- [1..n], mod n x == 0]