-- decimo setimo problema do projeto Euler

import Data.List
import Data.Maybe

{--

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

--}

mapa1 :: [(Int, String)]
mapa1 = [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five"),(6,"six"),(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"eleven"),(12,"twelve"),(13,"thirteen"),(14,"fourteen"),(15,"fifteen"),(16,"sixteen"),(17,"seventeen"),(18,"eighteen"),(19,"nineteen")]

mapaD :: [(Int, String)]
mapaD = [(2, "twenty"),(3,"thirty"),(4,"forty"),(5,"fifty"),(6,"sixty"),(7,"seventy"),(8,"eighty"),(9,"ninety")]

procuraMapa :: Int -> [(Int, String)] -> String
procuraMapa num m = fromJust (lookup num m)

escreveDecimal :: Int -> String
escreveDecimal num
           | num < 20 = procuraMapa num mapa1
           | uni == 0 = procuraMapa deci mapaD
           | otherwise = procuraMapa deci mapaD ++ "-" ++ procuraMapa uni mapa1
           where deci = div num 10
                 uni = mod num 10

escreveCentena :: Int -> String
escreveCentena num
               | deci == 0 = procuraMapa cen mapa1  ++ " hundred"
               | otherwise = procuraMapa cen mapa1 ++ " hundred and " ++ escreveDecimal deci
               where cen = div num 100
                     deci = mod num 100

escreveMilhar :: Int -> String
escreveMilhar num
                  | cen == 0 = procuraMapa mi mapa1 ++ " thousand"
                  | otherwise = procuraMapa mi mapa1 ++ " thousand and " ++ escreveCentena cen
                    where mi = div num 1000
                          cen = mod num 1000

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

extenso num
          | nDigito < 3 = escreveDecimal num
          | nDigito == 3 = escreveCentena num
          | nDigito == 4 = escreveMilhar num
          where nDigito = (length . digs) num

solucao1 = sum $ map (length . escreveDecimal) [1,2,3,4,5]

solucao =   sum $ map (length . (filter (\x -> elem x ['a'..'z'])) . extenso) [1..1000]
