module Practica02 where

-- | reversa_tr: Función auxiliar recursiva de cola para invertir una lista.
-- | Recibe la lista a invertir y un acumulador (lst) que almacena el resultado invertido.
-- | Cuando la lista de entrada está vacía, devuelve el acumulador.
reversa_tr :: [a] -> [a] -> [a]
reversa_tr [] lst = lst
reversa_tr (hd : tl) lst = reversa_tr tl (hd : lst)

-- | reversa: Invierte el orden de los elementos de una lista.
-- | Utiliza la función auxiliar 'reverse_tr' con un acumulador inicial vacío.
reversa :: [a] -> [a]
reversa lst = reversa_tr lst []

data Bit = O | I
  deriving (Show, Eq)

type Binario = [Bit]

-- | toDecimal: Convierte un número binario a su valor entero decimal.
-- | Invierte la lista de bits antes de la conversión para procesar del bit menos significativo al más significativo.
toDecimal :: Binario -> Int
toDecimal bin = aux (reversa bin)
  where
    -- | aux: Función auxiliar que realiza la conversión decimal recursivamente.
    -- | Recibe el binario invertido.
    -- | El valor actual se multiplica por 2 en cada paso y se suma el valor del bit (0 para O, 1 para I).
    aux [] = 0
    aux (O : tl) = 2 * (aux tl)
    aux (I : tl) = 2 * (aux tl) + 1

-- | lastDigitToBit: Función auxiliar que convierte el último dígito de un entero a un 'Bit'.
-- | Si el entero es impar (resto 1), devuelve 'I'. Si es par (resto 0), devuelve 'O'.
lastDigitToBit :: Int -> Bit
lastDigitToBit n
  | (mod n 2) == 1 = I
  | otherwise = O

-- | toBin: Convierte un número entero positivo a su representación binaria.
-- | Utiliza la función auxiliar 'toBin_tr' con un acumulador inicial vacío.
toBin :: Int -> Binario
toBin n = toBin_tr n []

-- | toBin_tr: Función auxiliar recursiva de cola para la conversión de entero a binario.
-- | Recibe el entero a convertir y un acumulador (bin) para construir el binario.
-- | En cada paso, agrega el bit menos significativo ('lastDigitToBit n') al frente del acumulador
-- | y divide el entero por 2 ('div n 2') para el siguiente paso.
toBin_tr :: Int -> Binario -> Binario
toBin_tr 0 bin = bin
toBin_tr n bin = toBin_tr (div n 2) ((lastDigitToBit n) : bin)

-- | suma_bits: Función auxiliar que realiza la suma binaria de tres bits: dos bits de entrada (x, y) y un acarreo de entrada (z).
-- | Devuelve una tupla: (Bit de Suma, Bit de Acarreo de salida).
-- | Se basa en la tabla de verdad de un sumador completo.
suma_bits :: Bit -> Bit -> Bit -> (Bit, Bit)
suma_bits I I z = (z, I)
suma_bits I y I = (y, I)
suma_bits x I I = (x, I)
suma_bits x O O = (x, O)
suma_bits O y O = (y, O)
suma_bits O O z = (z, O)

-- | suma: Suma dos números binarios.
-- | La suma se realiza bit a bit de derecha a izquierda usando 'suma_bits'.
-- | Las listas de bits se invierten al principio y el resultado se invierte al final para mantener el orden correcto.
suma :: Binario -> Binario -> Binario
suma bin1 bin2 = reversa (aux (reversa bin1) (reversa bin2) O)
  where
    -- | aux: Función auxiliar recursiva para la suma de binarios.
    -- | Recibe los binarios invertidos y el acarreo de entrada.
    -- | Maneja los casos en los que una de las listas se vacía y el caso de acarreo final.
    aux x [] O = x
    aux [] y O = y
    aux x [] I = aux [I] x O
    aux [] y I = aux [I] y O
    aux (xh : x_tl) (yh : y_tl) carry = bit_sum : (aux x_tl y_tl new_carry)
      where
        -- | Realiza la suma de los bits actuales (xh, yh) con el acarreo de entrada (carry).
        (bit_sum, new_carry) = suma_bits xh yh carry


-- | palindromo: Comprueba si una lista es un palíndromo.
-- | Requiere que los elementos de la lista soporten la comparación de igualdad ('Eq a').
-- | Compara la lista original con su versión invertida, elemento a elemento.
palindromo :: Eq a => [a] -> Bool
palindromo lst = aux lst (reversa lst)
  where
    -- | aux: Función auxiliar que compara la lista original con la invertida.
    aux [] [] = True
    aux (hd : tl) (rvhd : rvtl) = (hd == rvhd) && (aux tl rvtl) -- Compara cabezas y sigue con colas.
    aux _ _ = False

-- | belongs: Función auxiliar que comprueba si un elemento 'x' pertenece a una lista 'lst'.
-- | Requiere que los elementos soporten la comparación de igualdad ('Eq a').
belongs :: Eq a => a -> [a] -> Bool
belongs _ [] = False
belongs x (hd : tl) = (x == hd) || belongs x tl

-- | diferenciaSimetrica: Calcula la diferencia simétrica de dos listas, 'l1' y 'l2'.
-- | El resultado son los elementos que están en 'l1' pero no en 'l2',
-- | más los elementos que están en 'l2' pero no en 'l1' (la unión menos la intersección).
-- | Utiliza 'listas por comprensión' y la función 'belongs'.
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica l1 l2 = [x | x <- l1, not (belongs x l2)] ++ [x | x <- l2, not (belongs x l1)]

-- | conjuntoPotencia: Calcula el conjunto potencia de una lista.
-- | Devuelve una lista de listas, donde cada lista interna es un subconjunto de la lista original.
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (hd : tl) = [hd : x | x <- conjuntoPotencia tl] ++ conjuntoPotencia tl
  -- El resultado es la unión de:
  -- 1. Los subconjuntos que incluyen el elemento 'hd' (obtenidos añadiendo 'hd' a cada subconjunto de 'tl').
  -- 2. Los subconjuntos que no incluyen el elemento 'hd' (el conjunto potencia de 'tl').


type ListaPar a b = [(a, b)]

-- | longitud: Calcula la longitud de una 'ListaPar a b'.
-- | Cada par cuenta como 2 elementos, por lo que la longitud real es el doble del número de pares.
longitud :: ListaPar a b -> Int
longitud [] = 0
longitud (_ : tl) = 2 + longitud tl

-- | myMap: Aplica una función 'f' al primer elemento de cada par y una función 'g' al segundo elemento.
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d
myMap _ _ [] = []
myMap f g ((x, y) : tl) = (f x, g y) : (myMap f g tl)

-- | sumaPares: Suma todos los primeros elementos de los pares (x) y todos los segundos elementos (y) por separado.
-- | Devuelve un único par (suma_x, suma_y).
-- | Requiere que los tipos 'a' y 'b' sean tipos numéricos ('Num a', 'Num b').
sumaPares :: (Num a, Num b) => ListaPar a b -> (a, b)
sumaPares [] = (0, 0)
sumaPares ((x, y) : tl) = (x + sum_x, y + sum_y)
  where
    (sum_x, sum_y) = sumaPares tl

-- | myFilter: Filtra los pares en la 'ListaPar a b' basándose en un predicado 'f' que opera sobre el par completo.
-- | Si el predicado 'f' es verdadero para un par, lo incluye en la lista resultante.
myFilter :: ((a, b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter _ [] = []
myFilter f (hd : tl) = if (f hd) then (hd : (myFilter f tl)) else (myFilter f tl)
