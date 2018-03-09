-- trigesimo nono problema do projeto Euler

import Data.List

{--

If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

--}

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

pitagoras a b c =  a^2 + b^2 == c^2
perimetro a b c = a + b + c

maximiza l = snd $ maximum $ map (\x -> (length x, head x)) l

-- solucao muito lenta
listaSolucoes =  [(a,b,c)| a <-[10..999], b <-[10..999], c <-[10..999], pitagoras a b c, a <= b , b < c, let p = perimetro a b c, p <= 1000, even p, a < p `div `3]

listaSolucoes' =  [(a,b,c)| a <-[1..999], b <-[1..], c <-[1..], pitagoras a b c, a <= b , b < c, a + b < 1000, a + b + c < 1001]


-- solucao aritimetica, uso a uniao das funcoes pitagoras e perimetro para procurar somente o valor de b
-- as solucoes para esta equacao entao sao agrupadas e depois a que tiver maior numero de aparicoes eh a solucao do problema
solucaoAri = maximiza $ group [p | p <-[2,4..1000], a <-[2..p `div` 3], p * (p - 2  * a) `mod` (2*(p -  a)) == 0]
