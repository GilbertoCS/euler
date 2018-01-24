-- vigesiom sexto problema do projeto Euler

import Data.List
import Data.Ord
{--

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

--}

-- solucao envolve http://mathworld.wolfram.com/RepeatingDecimal.html
-- nocao de repeticao da mantissa
-- o a solucao envolve a seguinte lista http://oeis.org/A051626

maiorPeriodo :: [Int] -> Int
-- Em uma lista de inteiros que representam os periodos da divisao de numeros 1/n, acha qual o maior valor da lista
maiorPeriodo = maximum
--ex: [0, 0, 1, 0, 0, 1, 6, 0, 1, 0, 2, 1, 6, 6, 1, 0, 16, 1] == 16

achaPeriodo :: Int -> Int -> [Int] -> [Int]
-- Para um dado numero positivo Int (denominador da divisao 1/n) retorna uma lista com os digitos de um ciclo de mantissa [Int] - periodo

{-- ex: 1/7
1 mod 7 = 1
10 mod 7 = 3
30 mod 7 = 2
20 mod 7 = 6
60 mod 7 = 4
40 mod 7 = 5
50 mod 7 = 1
--}
--solucao funciona, mas poderia ser mais eficiente, parece errada comparada com a funcao remainders
achaPeriodo n d l
                 | n * 10 `mod` d == 0 = []
                 | length l == d - 1 = nub l -- partindo da premissa que o tamanho do periodo é igual a d-1
                 | otherwise = achaPeriodo (resto * 10) d noval
                  where resto = n `mod` d
                        noval = resto:l

tamanhoPeriodo n = length $ achaPeriodo 1 n []

sol1 = map recurringCycle [1..1000]
solucao = elemIndex (maiorPeriodo sol1) sol1
--solucao precisa somar 1 pois o indice começa com zero, logo a posicao para a solucao fica uma unidade a mais que o indice encontrado

-- funcao de https://wiki.haskell.org/Euler_problems/21_to_30
recurringCycle d = remainders d 10 []
remainders d 0 rs = 0
remainders d r rs = let r' = r `mod` d
             in case elemIndex r' rs of
                                    Just i  -> i + 1
                                    Nothing -> remainders d (10*r') (r':rs)
