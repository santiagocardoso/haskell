module Lista1 where

{-
1. Declare uma função que receba as 3 medidas dos lados de um triangulo, a função deve 
informar se as medidas podem formar um triângulo Retornando a True em caso 
afirmativo e False caso contrário, por exemplo:
-}
ehTriangulo :: Int -> Int -> Int -> Bool
ehTriangulo a b c =
  if a + b > c && a + c > b && b + c > a then True else False

{-
2. Declare uma função que receba 3 medidas válidas dos lados de um triângulo e retorne 
se esse triângulo é equilátero, isósceles ou escaleno. O retorno deve ser uma String 
contendo a classificação do triângulo, por exemplo:
-}
tipoTriangulo :: Int -> Int -> Int -> String
tipoTriangulo a b c =
  if a == b && b == c then
    "equilatero"
  else 
    if a /= b && b /= c && a /= c then
      "escaleno"
    else 
      "isosceles"

{-
3. Declare uma função que receba as 3 medidas dos lados de um triângulo e retorne se 
essas medidas formam um triângulo, em caso afirmativo a função deve retornar o tipo 
do triângulo: equilátero, isósceles ou escaleno, caso contrário deve retornar a string: “não 
eh um triangulo”, use as funções declaradas anteriormente.
-}
triangulo :: Int -> Int -> Int -> String
triangulo a b c =
  if ehTriangulo a b c == True then
    tipoTriangulo a b c
  else
    "nao eh um triangulo"

{- 
4. Declare uma função que receba como parâmetro um inteiro n e retorne a soma dos 
números pares entre 0 e n.
-}
somaPares :: Int -> Int
somaPares 0 = 
  0
somaPares 1 =
  0
somaPares n = 
  if n `mod` 2 == 0 then
    n + somaPares (n - 2)
  else
    somaPares (n - 1)

{-
5. Declare uma função que receba inteiros (m e n) e retorne a seguinte série: 
2^0m + 2^1m + 2^2m + ... + 2^nm. Por exemplo:
-}
somaPot2m :: Int -> Int -> Int
somaPot2m 0 _ =
  0
somaPot2m m 0 =
  m
somaPot2m m n =
  (m * (2^n)) + somaPot2m m (n - 1)

{-6. Declare uma função que receba um número e retorne True caso o número seja primo e 
False caso contrário. Um número primo é um número natural maior que 1, e que possui 
apenas dois divisores: 1 e ele mesmo. Por exemplo
-}
fatores :: Int -> [Int]
fatores n = 
  [f | f <- [1..n], n `mod` f == 0]

primo :: Int -> Bool
primo n =
  fatores n == [1,n]

{-
7. Uma aproximação para o valor de π pode ser obtida por meio da série:
4/1 – 4/3 + 4/5 – 4/7 + 4/9 – 4/11 + ...
Declare uma função chamada seriePI que receba como parâmetro um inteiro n e 
calcule o valor da serie enquanto o termo for maior que 4/n. Execute os seguintes 
testes:
-}
seriePI :: Int -> Double
seriePI 0 =
  4
seriePI n =
  ((4 * ((-1)^fromIntegral(n))) / (1 + (2 * fromIntegral(n)))) + seriePI (n-1)
