-- trigesimo sexto problema do projeto Euler

import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
{--

The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)

--}

-- verifica se um numero n é palindrome
-- usei a opcao de transformar o numero em string para manipular mais rapido
ehPalindrome n = n == reverse n

decParaBin n = showIntAtBase 2 intToDigit n ""


-- fazer a conversao de string para numero somente na construcao e nao na propria funcao auxiliar
-- retorna o resultado completo, tentei fazer a conversao direto nas funcoes e a resposta foi a metade!
solucao = sum [a | a <- [1..999999], ehPalindrome (show a), ehPalindrome (decParaBin a)]
