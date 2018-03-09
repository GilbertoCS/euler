-- quadragesimo problema do projeto Euler

import Data.Char
{--

An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

--}

-- para gerar mais facil a concatenacao transformei os numeros em uma grande string
listaDecimaisCons = concatMap show [1..]

-- subtrai um pois a lista tem indice com comeco em zero
nthDigito n = digitToInt $ listaDecimaisCons !! (n - 1)

solucao = product [nthDigito 1, nthDigito 10, nthDigito 100, nthDigito 1000, nthDigito 10000, nthDigito 100000, nthDigito 1000000]
