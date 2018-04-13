-- quiquagesimo problema do projeto Euler

import           Data.Function       (on)
import           Data.List
import           Data.Numbers.Primes
{--

The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?

--}
-- adaptado de  https://zach.se/project-euler-solutions/50/

consecutive :: Int -> Int -> [Int]
-- gera lista de somas acumuladas dada uma lista p de numeros primos, o tamanho da lista é n
consecutive n p = dropWhile (not . isPrime) $ reverse sums
      where
            sums = takeWhile (< n) $ scanl1 (+) $ dropWhile (< p) primes

solucao :: Int
solucao = head $ maximumBy (compare `on` length) $ map (consecutive 1000000) $ take 10 primes -- 10 porque as listas de somas cumulativas estabilizam seu tamanho com 10 elementos

main :: IO ()
main = print solucao
