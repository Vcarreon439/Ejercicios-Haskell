------Practica 1

----Ejercicio 1.1

--Con condicionales:
fact1 :: Integer -> Integer
fact1 n = if n == 0 then 1
else n * fact1 (n-1)

--Mediante guardas:
fact2 :: Integer -> Integer
fact2 n
    | n == 0 = 1
    | otherwise = n * fact2 (n-1)

--Restricción del dominio mediante guardas
fact4 :: Integer -> Integer
fact4 n
    | n == 0 = 1
    | n >= 1 = n * fact4 (n-1)

----Ejercicio 1.3.

--Usando la predefinida odd
impar1 :: Integer -> Bool
impar1 = odd

--Usando las predefinidas not y even:
impar2 :: Integer -> Bool
impar2 x = not (even x)

--Usando las predefinidas not, even y (.):
impar3 :: Integer -> Bool
impar3 = not . even

----Ejercicio 1.4.

--Multiplicacion
cuadrado_1 :: Num a => a -> a
cuadrado_1 x = x*x
--Cubo
cuadrado_2 :: Num a => a -> a
cuadrado_2 x = x^2
--Secciones:
cuadrado_3 :: Num a => a -> a
cuadrado_3 = (^2)

----Ejercicio 1.5.

--1. Con sum, map y cuadrado:
suma_de_cuadrados_1 :: [Integer] -> Integer
suma_de_cuadrados_1 l = sum (map cuadrado l)

--2. Con sum y listas intnsionales:
suma_de_cuadrados_2 :: [Integer] -> Integer
suma_de_cuadrados_2 l = sum [x*x | x <- l]

--3. Con sum, map y lambda:
suma_de_cuadrados_3 :: [Integer] -> Integer
suma_de_cuadrados_3 l = sum (map (\x -> x*x) l)

--4. Por recursión:
suma_de_cuadrados_4 :: [Integer] -> Integer
suma_de_cuadrados_4 [] = 0
suma_de_cuadrados_4 (x:xs) = x*x + suma_de_cuadrados_4 xs

--1.10. Anterior de un número natural
--Ejercicio 1.10. Definir la función anterior tal que anterior x es el anterior del número natural x. 

--1. Con patrones:
--anterior_1 :: Int -> Int
--anterior_1 (n + 1) = n

--2. Con guardas:   
anterior_2 :: Int -> Int
anterior_2 n | n > 0 = n-1

--3 Con condicionales
anterior_3 :: Integer -> Integer
anterior_3 n = if n == 0 then 0
else n-1

--Con patrones
anterior_4 :: Integer -> Integer
anterior_4 0 = 0
anterior_4 n = n-1

----Ejercicio 2.2. Definir la función siguiente tal que siguiente x sea el siguiente del número entero x. 
--1. Mediante sección:
siguiente_1 :: Integer -> Integer
siguiente_1 = (+1)
--2. Mediante instanciación parcial:
siguiente_2 :: Integer -> Integer
siguiente_2 = (+) 1
--3. Usaremos como siguiente la primera
siguiente :: Integer -> Integer
siguiente = siguiente_1
--4.
siguiente_4 :: Integer -> Integer
siguiente_4 x = x + 1

----Ejercicio 2.9. Redefinir la función filter tal que filter p l es la lista de los elementos de l que cumplen la propiedad p. 

--1. Definición por recursión:
n_filter_1 :: (a -> Bool) -> [a] -> [a]
n_filter_1 p [] = []
n_filter_1 p (x:xs) | p x = x : n_filter_1 p xs
    | otherwise = n_filter_1 p xs
--2. Definición con listas intensionales:
n_filter_2 :: (a -> Bool) -> [a] -> [a]
n_filter_2 p xs = [ x | x <- xs, p x ]


----Ejercicio 2.10. Redefinir la función sum tal que sum l es la suma de los elementos de l.
--1. Definición recursiva:
n_sum_1 :: Num a => [a] -> a
n_sum_1 [] = 0
n_sum_1 (x:xs) = x + n_sum_1 xs
--2. Definición con plegado:
n_sum_2 :: Num a => [a] -> a
n_sum_2 = foldr (+) 0

----Ejercicio 2.17. Definir la función factoriales tal que factoriales n es la lista de los factoriales desde el factorial de 0 hasta el factorial de n. Por ejemplo,

--1. Definición recursiva
--factoriales_1 :: Integer -> [Integer]
--factoriales_1 n =
--    reverse (aux n)
--    where aux 0 = [1]
--        aux (n+1) = (factorial (n+1)) : aux n
--2. Definición recursiva con acumuladores:
factoriales_2 :: Integer -> [Integer]
factoriales_2 n =
    reverse (aux (n+1) 0 [1])
    where aux n m (x:xs) = if n==m then xs
            else aux n (m+1) (((m+1)*x):x:xs)
--3. Definición con listas intensionales:
factoriales_3 :: Integer -> [Integer]
factoriales_3 n =  [fact1 x | x <- [0..n]]
--4. Definición con map:
factoriales_4 :: Integer -> [Integer]
factoriales_4 n = map fact1 [0..n]
--5. Definición con scanl:
factoriales_5 :: Integer -> [Integer]
factoriales_5 n = scanl (*) 1 [1..n]

----Ejercicio 2.21. Definir la función divisible tal que divisible x y se verifica si x es divisible por y

divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0

divisible2 :: Int -> Int -> Bool
divisible2 x y = divisible x y

divisible3 :: Int -> Int -> Bool
divisible3 x y = if (x `mod` y) == 0 then True
else False

divisible4 :: Int -> Int -> Bool
divisible4 x y
    | (x `mod` y) == 0 = True
    | otherwise = False


----Ejercicio 2.22. Definir la función divisores tal que divisores x es la lista de los divisores de x.
--1. Mediante filtro:
divisores_1 :: Int -> [Int]
divisores_1 x = filter (divisible x) [1..x]
--2. Mediante comprensión:
divisores_2 :: Int -> [Int]
divisores_2 x = [y | y <- [1..x], divisible x y]
--3. Equivalencia de las definiciones:
prop_equivalencia_1_2 :: Int -> Bool
prop_equivalencia_1_2 x =
    divisores_1 x == divisores_2 x  

--4. Usaremos como divisores la segunda
divisores :: Int -> [Int]
divisores = divisores_2

----Ejercicio 2.23. Definir la función primo tal que primo x se verifica si x es primo.
primo :: Int -> Bool
primo x = divisores x == [1,x]


----Ejercicio 2.24. Definir la función primos tal que primos x es la lista de los números primos menores o iguales que x.

--1. Mediante filtrado:
primos_1 :: Int -> [Int]
primos_1 x = filter primo [1..x]
--2. Mediante comprensión:
primos_2 :: Int -> [Int]
primos_2 x = [y | y <- [1..x], primo y]

----Ejercicio 2.25. Definir la función día tal que dia d m a es el día de la semana correspondiente al día d del mes m del año a.

día :: Int -> Int -> Int -> String
día d m a = díaSemana ((númeroDeDías d m a) `mod` 7)

númeroDeDías :: Int -> Int -> Int -> Int
númeroDeDías d m a = (a-1)*365
    + númeroDeBisiestos a
    + sum (take (m-1) (meses a))
    + d

númeroDeBisiestos :: Int -> Int
númeroDeBisiestos a = length (filter bisiesto [1..a-1])


bisiesto :: Int -> Bool
bisiesto a =
    divisible a 4 && (not(divisible a 100) || divisible a 400)

meses a = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    where feb | bisiesto a = 29
            | otherwise = 28

díaSemana :: (Eq a, Num a) => a -> String
díaSemana 0 = "domingo"
díaSemana 1 = "lunes"
díaSemana 2 = "martes"
díaSemana 3 = "miércoles"
díaSemana 4 = "jueves"
díaSemana 5 = "viernes"
díaSemana 6 = "sábado"


----Realiza un programa en Haskell que le pida al usuario su fecha de nacimiento y le devuelva  en letras. 
--ejemplo
--Entrada: 12/03/1989
--Salida: 12 de marzo de 1989

strmes :: Int -> String
strmes m 
    | m == 1 = "enero"
    | m == 2 = "febrero"
    | m == 3 = "marzo"
    | m == 4 = "abril"
    | m == 5 = "mayo"
    | m == 6 = "junio"
    | m == 7 = "julio"
    | m == 8 = "agosto"
    | m == 9 = "septiembre"
    | m == 10 = "octubre"
    | m == 11 = "noviembre"
    | m == 12 = "diciembre"

strfecnac :: Int -> Int -> Int -> String
strfecnac d m a = show d  ++ " de " ++ strmes m ++ " de " ++ show a 