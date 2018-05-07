-- quiquagesimo primeiro problema do projeto Euler

import           Data.Numbers.Primes
{--
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

--}

{--
passos para solução:
1 obter lista de números primos
2 filtrar esta lista com somente números com 8 digitos
3 filtrar esta lista para conter somente números que terminem com 3
4 filtrar esta lista para conter somente números com digitos repetidos
5 para cada número gerar uma lista de números candidatos a família de primos
6 verificar se todos os candidatos são primos
 - se sim então pegar o primeiro número da lista de canditados
 - senão executar o passo 5 para o próximo número da lista
--}

-- próximas tres funcoes foram adaptadas de https://wiki.haskell.org/Euler_problems/51_to_60
trocaDigito :: Char -> Char -> Char
-- troca número para 0 ou 1
trocaDigito d c
                  | {--c =='0' ||--} c == '1' = d -- para este problema a solucao nao começa com zero entao basta  verificar se o digito é 1
                  | otherwise = c

proximoDigito :: (Show a1, Read a, Num a) => (Char -> Char) -> a1 -> a
proximoDigito repl l = (+ 0) $ read $ map repl $ show l

-- PASSO 5
sequencias :: (Num t, Read t, Show a) => a -> [t]
-- gera sequencia a partir de um numero, somando 1 ao digito repetido, que é definido pela funcao trocaDigito, para formar os numero subsequentes
-- para este problema a sequencia nao precisa comecar com digitos em 0
sequencias n = [proximoDigito (trocaDigito a) n |a<-['1'..'9']]

sequenciasPrimos :: Integer -> [Integer]
sequenciasPrimos n = filter isPrime $ sequencias n

solucao :: Integer
solucao = head [head a| a<- map sequenciasPrimos primes, length a == 8]

main :: IO ()
main = print solucao
{--

exemplo de como procurar somente os elementos semelhantes para depois filtrar os outros numeros da familia
filter (\x -> possui2 '0' x) primos5S

soRepetidos l = sort $ l \\ (nub l)
soRepetidos [1,2,3,3,2,4,5,6,5] == [2,3,5]

[a| a<-['0'..'9'], possuiRepetidos a "11242"]

filtra os itens da lista que tem mais de k ocorrencias
filt k = map head . filter ((>=k) . length) . group . sort

para teste
["00017","11017","33317","00037","00449","22217"]

contar quantos elementos iguais em duas listas, melhor retirar os duplicados antes
let a = Set.fromList ["t","k","m"]
let b = Set.fromList ["k","b","t","c"]
print $ Set.size (a `Set.intersection` b)

length $ filter (`elem` b) a
--}


