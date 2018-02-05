-- vigesimo oitavo problema do projeto Euler

import Data.List
{--

Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

--}

-- a partir de uma lista de numeros impares, extrair grupos de 4 numeros com espacamento crescente e progressivo de uma unidade
f :: Num a => a -> a -> a
f i n = 4*i + n

-- se condiserar o problema como quadrados que formam uma espiral, então o primeiro quadradro seria representado por q, o numero do canto direito superiro seria (2q + 1)^2, porque cada lado do quadro mede 2q+1
-- para o proximo canto pegasse o valor o canto anterior menos o tamanho da lateral, menos 1 porque os dois numeros estao incluidos no tamanho, tem se entao (2q+1)^2 - 2q.
-- a soma dos quatro cantos do quadrado seria entao 4(2q+1)^2 - 12q

solucao 0 = 1
solucao n = 4 * (2 * n + 1)^2 - 12 * n + solucao (n - 1)

-- para o exemplo n = 2
-- para o pedido n = 500
