-- quadragesimo setimo problema do projeto Euler

import           Data.List
import           Data.Numbers.Primes
{--

The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?

--}

contaFatoresDistintos :: Int -> Int
contaFatoresDistintos n = length $ nub $ primeFactors n

consecutivos4 :: [Int] -> [(Int, Int, Int, Int)]
consecutivos4 l = [(x, y, z, w) | x <- l, let resto = tail l, y <- resto, z <- resto, w <- resto, (x + 1) == y, (y + 1) == z, (z + 1) == w]

consec4 :: (Num a, Eq a) => [a] -> [a]
consec4 l
       | (x +1) == y && (y+1) == z && (z+1) == w = l
       | otherwise = []
        where x = head l
              y = l !! 1
              z = l !! 2
              w = l !! 3

              -- funcao extraida de https://stackoverflow.com/questions/25098303/haskell-maximum-product-consecutive-digits-in-a-number
agrupaPorN :: Int -> [a] -> [[a]]
-- agrupa itens em grupos de n elementos ex: agrupaPorN 3 [1,2,4,0,3,1,3] => [[1,2,4],[2,4,0],[4,0,3],[0,3,1],[3,1,3]]
agrupaPorN n = takeWhile ((==n).length) . map (take n) . tails

-- solucao muito lenta dependendo do espaco de busca
--solucao :: [Int] -> Int -> (Int, Int, Int, Int)
--solucao l n = head $ consecutivos4 $ filter (\x -> contaFatoresDistintos x == n) l

solucao :: [Int] -> Int -> [Int]
solucao l n = head $ filter (not .null) $ map consec4 $ agrupaPorN n $ filter (\x -> contaFatoresDistintos x == n) l

main :: IO ()
main = print $ solucao [210..] 4
