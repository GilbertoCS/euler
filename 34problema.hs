-- trigesimo quarto problema do projeto Euler

{--

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

--}

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)
{--
fatorial :: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)
--}
fatorial n = product [1..n]

somaDeFatoriaisDe :: Int -> Int
somaDeFatoriaisDe n = sum $ map fatorial $ digs n

-- limite para a busca seria 2540160, pois este numero é 9!7 a explicacao matematica vem de http://www.mathblog.dk/project-euler-34-factorial-digits/

solucao  = sum [a | a <-[3..2540160], somaDeFatoriaisDe a == a]
