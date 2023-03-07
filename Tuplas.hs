primeiro :: (a, b) -> a
primeiro (x, y) = 
    x

segundo :: (a, b) -> b
segundo (x, y) =
    y

tamanho :: [a] -> Int
tamanho [] =
    0
tamanho (x:xs) =
    1 + tamanho xs

zipar :: [a] -> [b] -> [(a, b)]
zipar [] _ =
    []
zipar _ [] =
    []
zipar (x:xs) (y:ys) =
    (x, y) : zipar xs ys

-- zipar [1..] "abc"
-- zipar (1:[2..]) "abc"
-- (1, 'a') : zipar [2..] "bc"
-- (1, 'a') : zipar (2:[3..]) "bc"
-- (1, 'a') : (2, 'b') : zipar [3..] "c"
-- (1, 'a') : (2, 'b') : zipar (3:[4..]) "c"
-- (1, 'a') : (2, 'b') : (3, 'c') : zipar [4..] "" ---> Entra no 2º caso em vez de fazer a re-escrita como se fossem 4 situações
-- (1, 'a') : (2, 'b') : (3, 'c') : []

collatz :: Int -> [Int]
collatz n | n `mod` 2 == 0 =
    n : collatz (n `div` 2)
collatz n =
    n : collatz (3 * n + 1)

ones :: [Int]
ones =
    1 : ones