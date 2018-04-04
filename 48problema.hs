-- quadragesimo oitavo problema do projeto Euler


{--

The series, 11 + 22 + 33 + ... + 1010 = 10405071317.

Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

--}

somaSerie :: Integer
somaSerie = foldr  (\x a -> x^x +a ) 0 [1..1000]


-- da biblioteca Data.List.Extras
takeEnd :: Int -> [a] -> [a]
takeEnd i xs = f xs (drop i xs)
    where f (_:xxs) (_:ys) = f xxs ys
          f xss _          = xss

solucao :: String
solucao = takeEnd 10 (show somaSerie)
