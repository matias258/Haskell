--primero definimos esprimo

menordivdesde::Integer->Integer->Integer
menordivdesde n k | mod n k == 0 = k
                  | otherwise = menordivdesde n (k+1)

menordiv::Integer->Integer
menordiv n = menordivdesde n 2

esprimo ::Integer->Bool
esprimo 1 = False
esprimo n = n == menordiv n 

--despues el nesimoprimo

minimoPrimoDesde :: Integer->Integer
minimoPrimoDesde n | esprimo n = n
                   | otherwise = minimoPrimoDesde (n + 1)

nEsimoprimo :: Integer->Integer
nEsimoprimo 1 = 2
nEsimoprimo n = minimoPrimoDesde (1 + nEsimoprimo ( n - 1))


-----------------------ahora q tenemos la definicion de primos intentemos hacer los ejercicios---










