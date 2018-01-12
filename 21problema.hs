-- vigesimo primeiro problema do projeto Euler

--import Data.List
{--

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

--}

-- https://rosettacode.org/wiki/Proper_divisors#Haskell
divisores :: (Integral a) => a -> [a]
divisores n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]


amigavelDe numeroA1 = if soma numeroA2 == numeroA1 && numeroA1 /= numeroA2 then numeroA2 else 0
             where soma = sum . divisores
                   numeroA2 = soma numeroA1

solucao = sum $ filter (\x -> amigavelDe x > 0) [220..10000]
