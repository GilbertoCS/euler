-- trigesimo quinto problema do projeto Euler

import Data.List
import Data.Char
import Data.Numbers.Primes
{--
    The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

    There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

    How many circular primes are there below one million?
--}

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

joiner :: [Int] -> Int
joiner = read . concatMap show

listaPermutacoesDigitos :: Int -> [Int]
-- gera uma lista de permutacoes de algarismos de um numero n
listaPermutacoesDigitos n = map joiner $ permutations $ digs n

somentePrimos :: [Int] -> [Int]
-- dada uma lista de numero retorna somente aqueles que sao primos
somentePrimos = filter isPrime

ehPrimoCircular n = length lista > 1
                    where lista = somentePrimos $ listaPermutacoesDigitos n

ehPrimoCircular2 ls =  all isPrime lista
                       where lista = map (\x -> read x::Int) ls

circulaDigito :: String -> [String]
-- gera lista de permutacoes de s, sendo cada permutacao a troca do primeiro item da lista para o final da mesma
circulaDigito s = circulaDigito' s s

circulaDigito' :: String -> String -> [String]
circulaDigito' s r
              | rotacionado == r = r:[]
              | otherwise = rotacionado: circulaDigito' rotacionado r
              where rotacionado = tail s ++ [head s]


solucao = length $ filter ehPrimoCircular $ dropWhile (\x -> odd x && (x/=5) && x<1000000) primes
-- solucao muito mais eficiente
solucao2 = length $ 2: [a | a <- takeWhile (<1000000) primes, odd a, ehPrimoCircular2 $ circulaDigito $ show a]
