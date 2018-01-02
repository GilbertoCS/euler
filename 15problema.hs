-- decimo quinto problema do projeto Euler


{--

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?

--}

--problema de combinacao binomial
-- a formula seria C(2n n) onde n é o numero de celulas do grid, entao para um grid de 2x2 C(4 2)

-- binomial coefficient from https://gist.github.com/david-bergstrom/d0ede96a2e48d98cfe7a
choose n 1 = n
choose n k
    | k > n     = -1
    | k > n - k = choose n  (n - k)
    | otherwise = ((choose n (k - 1)) * (n - k + 1)) `div` k

