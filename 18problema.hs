-- decimo oitavo problema do projeto Euler

import Data.Matrix
{--
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

--}

-- ideia cada triangulo possui subtriangulos, somar as laterias de cada triangulo para saber que caminho percorrer para baixo

triangulo1 :: Matrix Int
triangulo1 = fromLists [[3,0,0,0],
                        [7,4,0,0],
                        [2,4,6,0],
                        [8,5,9,3]]

triangulo2 :: Matrix Int
triangulo2 = fromLists [[75,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                        [95,64,0,0,0,0,0,0,0,0,0,0,0,0,0],
                        [17,47,82,0,0,0,0,0,0,0,0,0,0,0,0],
                        [18,35,87,10,0,0,0,0,0,0,0,0,0,0,0],
                        [20,04,82,47,65,0,0,0,0,0,0,0,0,0,0],
                        [19,01,23,75,03,34,0,0,0,0,0,0,0,0,0],
                        [88,02,77,73,07,63,67,0,0,0,0,0,0,0,0],
                        [99,65,04,28,06,16,70,92,0,0,0,0,0,0,0],
                        [41,41,26,56,83,40,80,70,33,0,0,0,0,0,0],
                        [41,48,72,33,47,32,37,16,94,29,0,0,0,0,0],
                        [53,71,44,65,25,43,91,52,97,51,14,0,0,0,0],
                        [70,11,33,28,77,73,17,78,39,68,17,57,0,0,0],
                        [91,71,52,38,17,14,91,43,58,50,27,29,48,0,0],
                        [63,66,04,68,89,53,67,30,73,16,69,87,40,31,0],
                        [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]


-- com um triangulo a - topo b lado esquerdo e c lado direito
-- soma de a com o maior dos lados b ou c
maiorSoma tri = soma
                    where soma = topo + (maximum $ getRow 2 tri)
                          topo = getElem 1 1 tri

navega2 :: Matrix Int -> Int -> Int -> Int -> Int
navega2 tri n m s
                   | linhaAnterior == 1 = soma
                   | proximaCol == ncols tri = navega2 tri2 linhaAnterior 1 0
                   | proximaCol < ncols tri = navega2 tri2 n proximaCol 0
                   where proximaCol = m + 1
                         linhaAnterior = n - 1
                         subT = submatrix linhaAnterior n m proximaCol tri
                         soma = maiorSoma subT
                         tri2 = setElem soma (linhaAnterior, m) tri

