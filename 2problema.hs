-- segundo  problema do projeto Euler
{--

Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
--}

{--
x, y, sum = 1, 1, 0
while sum < 1_000_000
  sum += (x + y)
  x, y = x + 2*y, 2*x + 3*y
end

puts "Sum is #{ sum }."
--}


main = do
  --    [d] <- map read `fmap` getArgs
--      let n = read $ head d
        --printf "%f\n" (sumOfFib d)
        putStrLn $ " " ++ show (sumOfFib )


--fib :: Integer -> [Integer]
fib = 0 : scanl (+) 1 fib

--sumOfFib :: Int -> Integer
sumOfFib = sum $ filter (even) (takeWhile (<4000000) fib)
