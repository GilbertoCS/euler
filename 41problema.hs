-- quadragesimo primeiro problema do projeto Euler

import Data.List (sort)
import Data.Numbers.Primes
{--

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?

--}

  -- um numero pandigital primo com maior numero de digitos possui 7 digitos, pois qualquel numero divisivel por 3 é divisivel pela soma do divisores de 3, assim se somar um numero pandigital a soma sera divisivel por 3. Existe somente dois casos em que
-- isso nao funciona, para numeros pandigitais com 4 e 7 digitos, como o problema pede o maior...
-- para diminuir o espaco de busca, usei o limite de 7654321, pois ao gerar a lista de primos pandigitais este foi o maior valor

-- verifica se o numero n, em formato String, é pandigital com 7 digitos
-- na expressao abaixo o limite maior da lista representa o numero de digitos de um numero
pan7 :: String -> Bool
pan7 n = ['1'..'7'] == sort n

solucao = maximum $ filter (\x -> pan7(show x)) $ takeWhile (<7654321) primes
