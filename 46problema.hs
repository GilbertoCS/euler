-- quadragesimo sexto problema do projeto Euler

import           Data.Numbers.Primes
{--

It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

--}

compOdds :: [Int]
compOdds = filter (not . isPrime) [3, 5..]

verifConj :: Int -> Bool
verifConj n = any isPrime (takeWhile (>0) $ map (\i -> n - 2*i*i) [1..])

--[OC = p + 2 x (53^2)]
--l2 = [soma | p <-primes, n <-listaQua, soma<- p +2*n]
--solucao = [n | n<-[35,37..], p<-primes, isTwiceSquare (n - p), n >= p ]
solucao :: Int
solucao = head $ filter (not . verifConj) compOdds

main :: IO ()
main = print solucao
