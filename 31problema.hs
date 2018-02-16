-- trigesimo primeiro problema do projeto Euler

import Data.List
{--

In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?

--}

-- solucao de forca bruta, nem conseguiu terminar
-- solucao = length $ [(a,b,c,d,e,f,g) | a <- [0..200], b <- [0..100], c <- [0..40], d <- [0..20], e <- [0..10], f <- [0..4], g <- [0..2], a + 2*b + 5*c + 10*d + 20*e + 50*f + 100*g == 200]

-- http://www.algorithmist.com/index.php/Coin_Change

moedas = [1,2,5,10,20,50,100,200]

contaTrocado :: Int -> Int -> Int
contaTrocado n m
               | n < 0 || m < 0 = 0 -- sem solucao soma negativa de dinheiro
               | n == 0 = 1 -- uma solucao sem mais moedas para trocado, ou trocado para 0
               | otherwise = (contaTrocado n (m - 1) + contaTrocado (n - moedas !! m) m)

solucao = contaTrocado 200 (length moedas - 1)
