--Si vamos a trabajar con primos es mejor ya traer la definicion que usamos antes--

menordivdesde::Integer->Integer->Integer
menordivdesde n k | mod n k == 0 = k
                  | otherwise = menordivdesde n (k+1)

menordiv::Integer->Integer
menordiv n = menordivdesde n 2

esprimo ::Integer->Bool
esprimo 1 = False
esprimo n = n == menordiv n 

minimoPrimoDesde :: Integer->Integer
minimoPrimoDesde n | esprimo n = n
                   | otherwise = minimoPrimoDesde (n + 1)

nEsimoprimo :: Integer->Integer
nEsimoprimo 1 = 2
nEsimoprimo n = minimoPrimoDesde (1 + nEsimoprimo ( n - 1))

--Ejercicio 1---
--Definir la funcion longitud que dado un numero natural n, devuelve la longitud de la lista codificada por n. Notar que la lista no contiene ceros al final.
--Ej 132 = 2^2 . 3^1 . 5^0 . 7^0 . 11^1 = [2 , 1 , 0 , 0 , 1]
longitud:: Integer->Integer
longitud n = longitudDesde n 1

--necesiamos una funcion auxiliar que nos ayude a ir contando los primos

longitudDesde::Integer -> Integer ->Integer
longitudDesde 1 _ = 0
longitudDesde n k = 1 + (longitudDesde (div n (p^a))(k+1))
      where p = nEsimoprimo k
            a = quePotencialoDivide n p

quePotencialoDivide:: Integer->Integer->Integer
quePotencialoDivide n p | mod n p == 0 = 1 + quePotencialoDivide (div n p) p
                        | otherwise = 0 

--Ej 2: ) Definir la funcion iesimo que dados dos numeros naturales n e i, devuelve el iesimo elemento de la lista que codifica n. El primer elemento de la lista se corresponde con el ındice 1. Si el ındice esta fuera de rango, el programa devuelve 0.
--Por ej: el iesimo de 132 2 es 1 porque 1 es el 2do de la lista de 132


iesimo :: Integer-> Integer->Integer
iesimo n i = quePotencialoDivide n m
             where m = nEsimoprimo i









