-- decimo sexto problema do projeto Euler


{--

215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 21000?

--}


-- funcao do problema 4
digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

expo num exp = num ^ exp

-- expoente de numero, depois quebrar em lista, enfim soma os valores da lista
solucao = sum $ digs $ expo 2 1000
