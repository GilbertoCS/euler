-- quadragesimo segundo problema do projeto Euler

import Data.Char
--import Data.List (sort)
import Data.List.Utils (split, replace)
import System.IO

{--
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
--}

numeroTriangular :: Int -> Int
-- calcula o termo da posicao n na sequencia de numeros tringulares
numeroTriangular n = n * (n+1) `div` 2

valorNumerico :: Char -> Int
-- recebe um carater c e retorna um numero correspondente a ordem alfabetica de letras
-- a funcao ord usa a tabela de caracteres unicode, por isso subtrair 64 para obter a ordem: A = 1
valorNumerico c = ord c - 64

ehPalavraTriangular s = elem possivelTriangular listaTriangulares
                      where possivelTriangular = sum $ map valorNumerico s
                            listaTriangulares = map numeroTriangular [1..possivelTriangular]

-- converte as palavras separas por virgula em uma lista
formataNomes =  split "," . replace "\"" ""

main = do
         conteudo <- readFile "p042_words.txt"
         let listaNomes = formataNomes conteudo
         let listaPalavrasTriangulares = filter ehPalavraTriangular listaNomes
         let totalPalavras = length listaPalavrasTriangulares
         return totalPalavras
