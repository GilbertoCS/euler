-- quiquagesimo segundo problema do projeto Euler

import           Data.List
{--

It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

--}

allSame :: [String] -> Bool
-- Verifica se os elementos de uma lista sao todos iguais
-- para garantir a verificacao usei string de entrada para sortear os elementos
allSame []     = False
allSame lista = all (x ==) xs
                where (x:xs) = map sort lista

listaMultiplos :: Num b => b -> [b]
-- multiplica numero de entrada por uma lista predefinida para este problema
listaMultiplos n = map (n * ) [2,3,4,5,6]

-- verifica se um numero de entrada possui os mesmos digitos que uma lista de multiplos
-- a lista de multiplos foi convertida para string para facilitar a verificacao
mesmosDigitos n = allSame $ map show $ n : listaMultiplos n

solucao = head [n | n <- [1 ..], mesmosDigitos n]
--main :: IO ()
--main = print solucao
