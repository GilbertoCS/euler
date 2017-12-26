-- decimo problema do projeto Euler

import Data.Numbers.Primes

{--

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

--}

encontraSomaPrimosMenores = sum $ takeWhile (<2000000) primes
