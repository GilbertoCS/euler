-- primeiro problema do projeto Euler
-- Multiples of 3 and 5
-- Description:
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.


findSumOfMultiples35 l = sum $ filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) (init l)
-- sum [n | n <- [1..1000-1], n `mod` 5 == 0 || n `mod` 3 == 0]
