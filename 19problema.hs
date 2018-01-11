-- decimo nono problema do projeto Euler

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

{--


You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


--}

-- resposta é um nuemro de 1 a 7, sendo 1 segunda e 7 domingo
diaSemana ano mes dia = snd $ mondayStartWeek $ fromGregorian ano mes dia

numeroDomingos ano mes
                      | diaSemana ano mes tamanhoMes `elem` [1,2,7] = 5 -- final em 31
                      | diaSemana ano mes tamanhoMes `elem` [1,7] = 5 -- final em 30
                      | diaSemana ano mes tamanhoMes == 7 = 5 -- final em 29
                      | otherwise = 4
                       where tamanhoMes = gregorianMonthLength ano mes

-- quanto domingos existem no seculo 20
-- sum $ [ numeroDomingos a b| a <- [1901..2000], b <- [1..12] ]

primeiroDomingoMes ano mes = if diaSemana ano mes 1 == 7 then 1 else 0


-- quantos domingos foram dia primeiro em um dado ano durante todo seculo 20
solucao = sum [ primeiroDomingoMes a b| a <- [1901..2000], b <- [1..12] ]
