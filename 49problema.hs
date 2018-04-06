-- quadragesimo nono problema do projeto Euler

import           Data.List
import           Data.Numbers.Primes
{--

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?

--}

verificaPermitacao3 :: String -> String -> String -> Bool
-- se dois numeros, em formato de lista de caracteres, contem os mesmos digitos
verificaPermitacao3 n m z = sort n == sort m && sort n == sort z

achaTerceiro :: Int -> Int-> Int
achaTerceiro n m
                | n > m = 0 -- caso em que o primeiro numero é maior entao nao tem como encontrar o proximo em rodem crescente
                | otherwise = m + (m - n)

listaSeqs :: [Int]
listaSeqs = concat $ drop 1 $ take 2 [[a,b,c] | a <-[1000..9999], b <-[1000..9999], a < b, let c = achaTerceiro a b, verificaPermitacao3 (show a) (show b) (show c), isPrime a, isPrime b, isPrime c]

solucao :: String
solucao = concatMap show listaSeqs

main :: IO ()
main = print solucao
