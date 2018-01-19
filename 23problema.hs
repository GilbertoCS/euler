-- vigesimo terceiro problema do projeto Euler
import Data.List
import Data.List.Extra
{--

A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

--}

-- https://rosettacode.org/wiki/Proper_divisors#Haskell
divisores :: (Integral a) => a -> [a]
divisores n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

-- https://oeis.org/A048242

ehNumeroAbu :: Int -> Bool
-- verifica se o numero é abundante ou nao
ehNumeroAbu numero = sum (divisores numero) > numero

limiteSuperior :: Int
limiteSuperior = 20161

-- solucao
listaNumerosAbundantes :: [Int]
listaNumerosAbundantes = filter ehNumeroAbu [1..]
--ehSomaAbundantes :: [Int] -> Int -> Bool
ehSomaAbundantes num = any (\x -> ehNumeroAbu (num - x)) (takeWhile (<num) listaNumerosAbundantes)
solucao = sum $ filter (not . ehSomaAbundantes) [1..limiteSuperior]
--solucao = sum $ filter (not . ehSomaAbundantes listaNumerosAbundantes) [1..limiteSuperior]
