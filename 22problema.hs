-- vigesimo segundo problema do projeto Euler

import Data.List (sort)
import Data.List.Utils (split, replace)
import Data.Char
import System.IO
{--

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

--}

formataNomes =  split "," . replace "\"" ""

valorNumerico :: Char -> Int
-- recebe um carater c e retorna um numero correspondente a ordem alfabetica de letras
-- a funcao ord usa a tabela de caracteres unicode, por isso subtrair 64 para obter a ordem: A = 1
valorNumerico c = ord c - 64

valoresNumericos = map (sum . map valorNumerico)

calculaScore l = zipWith (*) [1..(length l)] l

main = do
         conteudo <- readFile "p022_names.txt"
         let listaNomes = sort $ formataNomes conteudo
         let listaNum = calculaScore $ valoresNumericos listaNomes
         let somaFinal = sum listaNum
         return somaFinal
