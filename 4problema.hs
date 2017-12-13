-- quarto  problema do projeto Euler
{--

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

--}

import Data.List

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

isPal n1 = digs n1 == (reverse . digs) n1

findLargestPalindrom = maximum $ filter isPal [ x * y | x <-[100..999] , y <- [100..999]]
