import System.IO
import Data.Char
import Data.List

type Doc     = String
type Linha   = String
type Palavra = String

--tirar pontuacao
limparPont :: Doc -> Doc
limparPont [] = []
limparPont (x:xs) =
  if isPunctuation x then
    limparPont xs
  else
    x : limparPont xs

--colocar minusculo
minusc :: Doc -> Doc
minusc [] = []
minusc (x:xs) =
  if isUpper x then
    toLower x : minusc xs
  else 
    x : minusc xs

--separar em linhas
lines' :: Doc -> [Linha]
lines' = lines

--numera as linhas
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas [] = []
numLinhas (x:xs) = aux 1 (x:xs)

aux :: Int -> [Linha] -> [(Int, Linha)]
aux c [] = []
aux c (x:xs) =
  (c, x) : aux (c+1) xs

--numera linha da palavra
separaPalavras :: [(Int, Linha)] -> [(Int, [Palavra])]
separaPalavras [] = []
separaPalavras ((x, ys):zs) =
  (x, words ys):separaPalavras zs

aux' :: (Int, [Palavra]) -> [(Int, Palavra)]
aux' (num, xs) = zip (repeat num) xs

numeraPalavras :: [(Int, [Palavra])] -> [(Int, Palavra)]
numeraPalavras = concatMap aux'

--remover palavras com < 3 letras
removerMenor :: [(Int, Palavra)] -> [(Int, Palavra)]
removerMenor [] = []
removerMenor ((x, ys):zs) =
  if length ys < 3 then
    removerMenor zs
  else
    (x, ys): removerMenor zs

--ordena alfabeticamente
ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar [] = []
ordenar ((x, ys):zs) = 
    let m = sort ((ys, x): map mudarordem zs) in
    map mudarordem m

mudarordem :: (a, b) -> (b, a)
mudarordem (a, b) = (b, a)

--junta as palavras
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar [] = []
agrupar ((x, ys):zs) = 
    let n = [x] ++ [fst n | n <- zs, snd(n) == ys ] in
    (n, ys): agrupar zs

--elimina repetidas
eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep [] = []
eliminarRep ((xs, ys):zs) =
    let o = [ (a,b) | (a,b) <- zs, ys /= b ] in
    (xs, ys) : eliminarRep o

construirIndice :: Doc -> [([Int], Palavra)]
construirIndice doc =
    let pontuacoes = limparPont doc in
    let minusculas = minusc pontuacoes in
    let linhas = lines' minusculas in
    let enumeradas = numLinhas linhas in
    let separa = separaPalavras enumeradas in
    let palavras = numeraPalavras separa in
    let removidas = removerMenor palavras in
    let ordenadas = ordenar removidas in
    let agrupadas = agrupar ordenadas in
    eliminarRep agrupadas

main :: IO ()
main = do
    putStrLn "Digite o nome do arquivo de entrada:"
    arquivo <- getLine
    h <- openFile arquivo ReadMode
    h <- openFile arquivo ReadMode
    texto <- hGetContents h
    print (construirIndice texto)
    hClose h
