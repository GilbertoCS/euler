-- decimo segundo problema do projeto Euler

import Data.Numbers.Primes
import Data.List
{--

The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

--}

-- primeira parte da solucao: gerar os numeros triangulares
-- n representa a ordem de um numero de triangulo
numeroTriangular n = n * (n+1) `div` 2

-- segunda parte da solucao: achar os divisores de um numero
--achar fatores primos
achaFatores num  = primeFactors num
-- somar 1 aos expoentes dos fatores
somaFatores l =  map ((+)1 . length) (group l)
-- multiplica exponentes somados
multiplicaExp l = foldl (*) 1 l

-- numero de divisores de um dado numero
numDivisores = multiplicaExp . somaFatores . achaFatores

-- terceira parte da solucao: gerar lista de numeros triangulares, depois encontrar seus divisores
-- e por fim selecionar somente o primeiro numero triangular que tenha 500 divisores
geraNumerosTriangulares =  map numeroTriangular [1..]
encontra500Divisores = filter (\x -> numDivisores x >= 500)

solucao = head $ encontra500Divisores $ geraNumerosTriangulares