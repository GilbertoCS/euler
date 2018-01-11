-- vigesimo problema do projeto Euler


{--

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

--}

fatorial 1 = 1
fatorial n = n * fatorial (n-1)

-- do problema 4
digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

solucao = sum $ digs $ fatorial 100
