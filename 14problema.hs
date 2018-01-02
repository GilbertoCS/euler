-- decimo quarto problema do projeto Euler

import Data.List

{--

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.


--}

solucao = maximum $ map tamanhoSeq [1000000,999999..10000]
--(525,837799)

-- determina tamanho de uma sequencia para o numInicial, saida par com o tamanho da sequencia e o numero inicial
tamanhoSeq numInicial = (length (geraLista numInicial), numInicial)

geraLista 1 = [1]
geraLista numInicial = numInicial : geraLista (separaNumero numInicial)

separaNumero :: Int -> Int
separaNumero num
      | even num = num `div` 2
      | otherwise  = num * 3 + 1
