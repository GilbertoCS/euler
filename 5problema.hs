-- quinto  problema do projeto Euler
{--

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

--}

-- gera lista de numeros divisiveis de acordo com a funcao dada por f no intervalo da lista que comeca em 2520 e pula de 2520 em 2520
numerosDivisiveis f = filter (\x -> f x == 0)[2520,5040..]

somaModulosdeN10 n = somaModulosdeN n [1..10]
-- soma das divisoes de um numero n na lista seguinte, simplificada para excluir os numeros que ja podem ser divididos
somaDivisivelN20 n = somaDivisivelN n [11,13,14,16,17,18,19,20]--[1..20]

-- solucao dividir um numero por cada outra da lista e ver se o resto é inteiro

somaDivisivelN n l = sum $ map (eDivisivel n) l
divisivelLista n l = map (eDivisivel n) l
-- divisao precisa com primeira parte numero inteiro e segunda o que vem depois da virgula
eDivisivel n d = (snd $ divMod n d)

somaModulosdeN n l = sum $ map (mod n) l
