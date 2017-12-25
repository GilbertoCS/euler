-- nono  problema do projeto Euler

import Data.List
{--

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

--}

-- funcao que gera lista de numer
os de pitagoras de 1 ate n, n é um numero natural
geraListaNPitagoras n = [(a,b,c)| a <- lista, b <- lista, c <- lista, a^2 + b^2 == c^2]
      where lista = [1..n]

encontraListaUnicaPitagoras lista = filter (\(a,b,c) -> a+b+c==1000) lista

produtoListaUnica lista = product lista
