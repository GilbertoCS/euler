-- trigesimo problema do projeto Euler

import Data.List
{--
    Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

    1634 = 14 + 64 + 34 + 44
    8208 = 84 + 24 + 04 + 84
    9474 = 94 + 44 + 74 + 44

    As 1 = 14 is not a sum it is not included.

    The sum of these numbers is 1634 + 8208 + 9474 = 19316.

    Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

--}

-- ideia: receber um numero com 4 algarismos, transformar em digitos, depois elevar a um expoente x a lista de digitos, somar os valores obtidos,
-- verificar se resultado e igual ao numero entrado no comeco

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

testaNumeroExpIgual :: Int -> Int -> (Int, Bool)
-- recebe um numero n e testa se a soma dos digitos desse numero elevado a exp sao iguais ao numero n
testaNumeroExpIgual exp n = (n , (somaDigitos $ elevaDigitos exp (transformaDigitos n)) == n)

transformaDigitos = reverse . digs
elevaDigitos exp l = map (^exp) l
somaDigitos = sum

solucaoTeste = filter (\x -> snd x == True) $ map (testaNumeroExpIgual 4) [1000..9999]
achaNumeros = filter (\x -> snd x == True) $ map (testaNumeroExpIgual 5) [2..355000] -- limite superior calculado como
-- x95 which gives us an x digit number. We can do this by hand. Since 95 = 59049, we need at least 5 digits. 595 = 295245, so with 5 digits we can make a 6 digit number. 6*95 = 354294. 
solucao = sum $ map (\par -> fst par) achaNumeros
