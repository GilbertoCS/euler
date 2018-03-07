-- trigesimo setimo problema do projeto Euler

import Data.Numbers.Primes
import Data.List
{--

The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

--}

-- trunca numero, em formato de string, da direita para esquerda, remove o numero de entrada e "" do inicio
-- ex: truncaDir "3797" == ["3","37","379"]
truncaDir = init . tail . inits

-- trunca numero, em formato de string, da esquerda para direita
truncaEsq = tail . init . tails

truncaAmbosLados :: String -> [String]
-- trunca o numero n, em formato String, por ambos os lados e devolve uma lista
truncaAmbosLados n = truncaDir n ++ truncaEsq n

--todosPrimos :: [a] -> Bool
-- verifica se todos os itens da lista l sao numeros primos
todosPrimos l = all isPrime l

stringsToInts s =  map (read::String->Int) s

numeroPrimoTruncado = todosPrimos . stringsToInts . truncaAmbosLados

-- na solucao usei break para pegar a lista de primos a partir do 11
solucao = sum $ take 11 $ [a | let lista = snd $ break (==11) primes, a <- lista, numeroPrimoTruncado (show a)]
