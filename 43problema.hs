-- quadragesimo terceiro problema do projeto Euler

import Data.List
{--

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.

--}

pan09 :: String -> Bool
-- verfifica se o numero n é pandigital de 0 a 9 digitos
-- uso de string para maior eficiencia e manipulacao dos itens da lista
pan09 n = ['0'..'9'] == sort n


-- limites obtidos de https://en.wikipedia.org/wiki/Pandigital_number
listaPandigitais = filter (pan09 . show)  [1406357289..9876543210] -- este espaco de problema é muito grande e lento
-- mudei o inicio do intervalo para 1406357289
-- d4d5d6 deve ser divisivel por 5 entao d6 deve ser ou 0 ou 5
-- d6d7d8 deve ser divisivel por 11, se d6 for 0 vai gerar trios que nao sao pandigitais (011,022) entao d6 é igual a 5
-- d7d8d9 deve ser divisivel por 13 leva a limitacao de combinacoes para  {5286, 5390, 5728, 5832}
-- d6d7d8d9d10  vai ter como possiveis valores {52867, 53901, 57289}

finais l = isSuffixOf "52867" l || isSuffixOf "53901" l || isSuffixOf "57289" l

listaPandigitaisdR = filter (finais . show) listaPandigitais

listaDivisores :: [Int]
-- ideia mapear a listaDivisores com a lista de listaTriosDigitos se todos da lista forem verdadeiros entao ok
listaDivisores = [1,2,3,5,7]

listaTriosDigitos n = chunksOf 3 (seq3 n)
                    where seq3 n
                               | length n == 3 = n
                               | otherwise = take 3 n ++ seq3 (tail n)

-- funcoes auxiliares do pacote Data.List.Split
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- converte lista de strings em Inteiros
stringToInt = map (read::String->Int) 

divisivelPor n p = mod n p == 0

-- verifica se todos os itens da lista sao verdadeiros
todosPositivos l = all (==True) l

listaDeTrios = map stringToInt $ map (take 5) $ map (listaTriosDigitos . show) listaPandigitaisdR

-- verifica se os elementos de uma lista de numeros sao divisiveis por uma lista de divisores 
divisiveis l = zipWith divisivelPor l listaDivisores

-- ideia testar a divibilidade somente dos primeiros trios
listaPares =  zip listaPandigitaisdR $ map (todosPositivos . divisiveis) listaDeTrios
solucao = sum $ map fst $ filter (\x -> snd x == True) listaPares

main = do
       putStr $ show solucao
