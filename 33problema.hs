-- trigesimo terceiro problema do projeto Euler

import Data.Ratio
{--

The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

--}

-- solucao vem da regra de simplificacao de fracoes, na qual existe um digito em comum que pode ser removido
-- para isso o numerador e denominador devem estar no intervalo 1..9, sendo n < d e o digito comum tambem esta no intervalo 1..9
-- a equacao que vai funcionar para este caso é 10n + dc / 10dc + d = n/d
-- 10 porque precisa separar os digitos dos numeros

formulas :: Int -> Int -> Int -> Bool
formulas numerador denominador digitoComum = denominador * (10 * numerador + digitoComum) == numerador * (10 * digitoComum + denominador)

listaNumDem =[(n,d) | i <- [1..9], d <-[1..i], n <-[1..d], d < i, n < d, formulas n d i]
produtoPares par = (product $ map fst par , product $ map snd par)
prods = produtoPares listaNumDem
solucao = denominador `div` (gcd numerador denominador)
          where denominador = snd prods
                numerador = fst prods
