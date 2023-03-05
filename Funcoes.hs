--1
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] =
    False
pertence x (y:ys) =
    if x == y then
        True
    else
        pertence x ys
    
--2
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] _ =
    []
intersecao (x:xs) ys =
    if x `pertence` ys then
        x : intersecao xs ys
    else
        intersecao xs ys

--3
concatena :: [a] -> [a] -> [a]
concatena [] (y:ys) =
    (y:ys)
concatena (x:xs) ys =
    x : concatena xs ys

inverso :: Ord a => [a] -> [a]
inverso [] =
    []
inverso (x:xs) =
    inverso xs `concatena` [x]

--4
pegar :: (Num a, Ord a) => a -> [a] -> [a]
pegar _ [] =
    []
pegar x (y:ys) = 
    if x > 0 then
        y : pegar (x-1) ys
    else
        []

nUltimos :: (Num a, Ord a) => a -> [a] -> [a]
nUltimos y [] =
    []
nUltimos y (x:xs) =
    inverso (pegar y (inverso (x:xs)))

--5
soma2 :: Num a => [a] -> [a] -> [a]
soma2 [] _ =
    []
soma2 _ [] =
    []
soma2 (x:xs) (y:ys) =
    x + y : soma2 xs ys

--6
pot2 :: Integral a => a -> [a]
pot2 0 = [1]
pot2 n = 
    [ 2 ^ x | x <- [1..n] ]

--7
maior :: Ord a => [a] -> a
maior [] =
    error "Lista vazia!"
maior [x] =
    x
maior (x:xs) =
    if x > maior xs then
        x
    else
        maior xs
    
intercalacao :: (Ord a, Num a) => [a] -> [a] -> [a]
intercalacao [] (y:ys) =
    (y:ys)
intercalacao (x:xs) [] =
    (x:xs)
intercalacao (x:xs) (y:ys) =
    if (menor (x:xs)) < (menor (y:ys)) then
        x : intercalacao xs (y:ys)
    else
        y : intercalacao (x:xs) ys

--8
menor :: Ord a => [a] -> a
menor [] =
    error "Lista vazia!"
menor [x] =
    x
menor (x:xs) =
    if x < menor xs then
        x
    else
        menor xs

--9
removerElem :: Ord a => a -> [a] -> [a]
removerElem _ [] =
    []
removerElem y (x:xs) =
    if x == y then
        xs
    else
        x : removerElem y xs

--10
ordenar :: Ord a => [a] -> [a]
ordenar [] =
    []
ordenar x =
    menor x : ordenar (removerElem (menor x) x)

--11
insereOrd :: (Num a, Ord a) => a -> [a] -> [a]
insereOrd y (x:xs) =
    if y `pertence` (x:xs) then
        (x:xs)
    else
        ordenar ([y] `intercalacao` (x:xs))

--12
enesimo :: (Num a, Ord a) => a -> [a] -> a
enesimo _ [] =
    error "lista vazia"
enesimo 1 (x:xs) =
    x
enesimo n (x:xs) =
    enesimo (n - 1) xs

--13
repetir :: (Num a, Ord a) => a -> a -> [a]
repetir 0 e =
    []
repetir n e =
    e : repetir (n - 1) e 
{-
repetir 4 10 =
10 : repetir 3 10 =
10 : 10 : repetir 2 10 =
10 : 10 : 10 : repetir 1 10 =
10 : 10 : 10 : 10 : repetir 0 10 =
10 : 10 : 10 : 10 : []
10 : 10 : 10 : 
-}

--14
removeTab :: [Char] -> [Char]
removeTab [] =
    []
removeTab (x:xs) =
    if x == '\t' then
        ' ' : removeTab xs
    else
        x : removeTab xs

--15
lower :: Char -> Char
lower 'A' = 'a'
lower 'B' = 'b'
lower 'C' = 'c'
lower 'D' = 'd'
lower 'E' = 'e'
lower 'F' = 'f'
lower 'G' = 'g'
lower 'H' = 'h'
lower 'I' = 'i'
lower 'J' = 'j'
lower 'K' = 'k'
lower 'L' = 'l'
lower 'M' = 'm'
lower 'N' = 'n'
lower 'O' = 'o'
lower 'P' = 'p'
lower 'Q' = 'q'
lower 'R' = 'r'
lower 'S' = 's'
lower 'T' = 't'
lower 'U' = 'u'
lower 'V' = 'v'
lower 'W' = 'w'
lower 'X' = 'x'
lower 'Y' = 'y'
lower 'Z' = 'z'

minusculas :: [Char] -> [Char]
minusculas [] =
    []
minusculas (x:xs) =
    if x `pertence` ['A'..'Z'] then
        lower x : minusculas xs
    else
        x : minusculas xs

--16
inversoDupla :: [(a, b)] -> [(b, a)]
inversoDupla [] =
    []
inversoDupla ((x, y):xs) =
    (y, x) : inversoDupla xs
--inversoDupla [(x, y)] =
--    [(y, x)]

--17
simetrico :: Eq a => [(a, a)] -> [Bool]
simetrico [] =
    []
simetrico ((x, y):xs) =
    if x == y then
        True : simetrico xs
    else
        False : simetrico xs

--18
numString :: Int -> [Char]
numString 0 = "0"
numString 1 = "1"
numString 2 = "2"
numString 3 = "3"
numString 4 = "4"
numString 5 = "5"
numString 6 = "6"
numString 7 = "7"
numString 8 = "8"
numString 9 = "9"
numString x =
    numString (x `div` 10) `concatena` numString (x `mod` 10)
--numString 126 =
--    126 div 10 concatena 126 mod 10
--      numString 12 : numString 6

--19
charn :: Char -> Int
charn '0' = 0
charn '1' = 1
charn '2' = 2
charn '3' = 3
charn '4' = 4
charn '5' = 5
charn '6' = 6
charn '7' = 7
charn '8' = 8
charn '9' = 9

stringNum :: [Char] -> Int
stringNum [] =
    0
stringNum (x:xs) =
    (charn x) * (10 ^ (length ((x:xs)) - 1)) + stringNum xs

--20
decBin :: Int -> [Char]
decBin 0 = 
    "0"
decBin 1 = 
    "1"
decBin x =
    decBin (x `div` 2) `concatena` decBin (x `mod` 2)
{-
--11 / 2 = sobra 1 = 5
-- 5 / 2 = sobra 1 = 2
-- 2 / 2 = sobra 0 = 1
-- 1 0 1 1      ^^^ <-
-}
 
--21
charb :: Char -> Int
charb '1' = 1
charb '0' = 0

binDec :: [Char] -> Int
binDec "0" =
    0
binDec "1" =
    1
binDec (x : xs) =
    (charb x) * (2)^(length ((x:xs)) - 1) + binDec xs
--1011
--1 * 2^3 + 0 * 2^2 + 1 * 2^1 + 1 * 2^0
--11

--22
trocoCafe :: (Integral a1, Num a2) => a1 -> a1 -> [(a2, a1)]
trocoCafe c p
    | z `div` 50 /= 0 = (50, (z `div` 50)) : trocoCafe 0 (z `mod` 50)
    | z `div` 20 /= 0 = (20, (z `div` 20)) : trocoCafe 0 (z `mod` 20)
    | z `div` 10 /= 0 = (10, (z `div` 10)) : trocoCafe 0 (z `mod` 10)
    | z `div` 5 /= 0  = (5, (z `div` 5))   : trocoCafe 0 (z `mod` 5)
    | otherwise =  []
    where z = p - c 
{-
trocoCafe 65 110 =
    110 - 65 = 45
-}
