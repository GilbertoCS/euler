-- vigesimo setimo problema do projeto Euler

import Data.List
import Data.Ord
import Data.Numbers.Primes
{--
Euler discovered the remarkable quadratic formula:

n2+n+41

It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39
. However, when n=40,402+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,412+41+41

is clearly divisible by 41.

The incredible formula n2−79n+1601
was discovered, which produces 80 primes for the consecutive values 0≤n≤79

. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n2+an+b

, where |a|<1000 and |b|≤1000

where |n|
is the modulus/absolute value of n
e.g. |11|=11 and |−4|=4

Find the product of the coefficients, a
and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.

--}

qf :: Int -> Int -> Int -> Int
qf n a b = n * (n + a) + b

qf2 n a b = isPrime $ abs $ qf n a b

-- b é primo, porque quando n=0, n² + an + b == b
-- Para n=1, n² + an + b pode ser escrita como: a = p - b - 1. Por isso pode usar todos os primos        < 1000+b+1 para obter o valor de a.
-- A solucao pede uma formula com mais numeros que a de Euler, que vai ate n=39, entao achar pares com numeros primos a partir de n=40
paresCoeficientes :: [(Int,Int)]
paresCoeficientes =  [(a,b) | b <- (takeWhile (<1000) primes), la <- takeWhile (<1000+b+1) primes, let a = la - b -1,
                            qf2 40 a b]

quantidadesPrimos pa = length $ takeWhile (\x -> qf2 x (fst pa) (snd pa)) [1..]

qntPares = snd $  maximumBy (comparing fst) [(quantidadesPrimos par, par) | par <- paresCoeficientes ]

solucao = (\(x,y) -> x * y) qntPares
-- solucao
-- gerar listas dos coeficientes, para o a valores entre -1000 e 1000 para b os primeiros 1000 primos
-- aplicar cada par de coeficientes a equacao e testar entre 1 a n quantos numeros primos consecutivos aparecem
-- escolher o par que gerar mais primos consecutivos
-- por fim multiplicar os coeficientes
