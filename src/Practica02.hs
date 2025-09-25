module Practica02 where

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

toDecimal :: Binario -> Int
toDecimal = undefined

toBin :: Int -> Binario
toBin = undefined

suma :: Binario -> Binario -> Binario
suma = undefined

--LISTAS

palindromo :: [a] -> Bool
palindromo = undefined

--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: [a] -> [a] -> [a]
diferenciaSimetrica = undefined

--Conjunto potencia
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined


--LISTAS DE LONGITUD PAR
--Esta va de regalo
type ListaPar a b = [(a,b)]

--Longitud
longitud :: ListaPar a b -> Int
longitud = undefined

--Map
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap = undefined

--Sumar pares
sumaPares :: ListaPar a b -> (a,b)
sumaPares = undefined

--Filter pares
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter = undefined