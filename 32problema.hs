-- trigesimo segundo problema do projeto Euler

import Data.List
{--
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

--}

--digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

nDigs :: Int -> Int
-- calcula o numero de digitos de um numero n, falta somar 1 ao valor obtido
nDigs = floor . logBase 10 . fromIntegral

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

joiner :: [Int] -> Int
joiner = read . concatMap show

pandig :: Int -> Bool
-- verifica se o numero n eh pandigital
pandig n = nDigs n == nDigs np
       where np = fromDigits $ nub (digs n)

pandigital x y p = [1..9] == sort (digs p ++ digs x ++ digs y)

takeEnd :: Int -> [a] -> [a]
takeEnd n xs = foldl' (const . drop 1) xs (drop n xs)

-- https://github.com/amcoder/euler-haskell/blob/master/euler0032.hs
-- limites foram tirados do site acima, sem explicao dos mesmos
-- pois sem esses limites a solucao iria demorar muito tempo

listaProdutos = [ a*b | a <- [1..50], b <- [100..2000], pandig $ joiner [a,b, a*b]  ] -- parece levar em considerao multiplicacoes duplicadas
listaProdutos2 = [ a*b | a <- [1..50], b <- [100..2000], pandigital a b (a*b) ] -- correto

solucao = sum . nub $  listaProdutos2
