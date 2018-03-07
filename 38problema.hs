-- trigesimo oitavo problema do projeto Euler

import Data.List (sort)

{--

Take the number 192 and multiply it by each of 1, 2, and 3:

    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

--}

pan9 :: String -> Bool
-- verifica se o numero n, em formato String, é pandigital com 9 digitos
-- na expressao abaixo o limite maior da lista representa o numero de digitos de um numero
pan9 n = ['1'..'9'] == sort n


produtoDis n l = map (* n) l
combina n = concatMap show n


-- o numero procurado vai ter no maximo 5 digitos, deve comecar com 9, entao o numero de digitos deve ser 4, pois
-- com 2 digitos da para gerar numeros com 8 digitos com a lista de 1,2,3 ou numeros com 11 digitos com a lista 1,2,3,4
-- com 3 digitos geramos numeros com 7 ou 11 digitos
-- o numero para multiplicar estaria entre 9000 e 9999  e a lista de multiplicadores seria [1,2], pois esta combinacao
-- gera numeros finais com 9 digitos

solucaoNumeroFixo = maximum [a | a <-[9000,9001..9999], pan9 $ combina $ produtoDis a [1,2]]
solucao = combina $ produtoDis solucaoNumeroFixo [1,2] 
