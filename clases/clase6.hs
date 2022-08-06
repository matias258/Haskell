---------------------Listas--------------------------


--acordemonos que la sumatoria que estudiamos se veia asi

sumatoria::Int->Int
sumatoria n | n == 0 = 0
            | otherwise = sumatoria (n-1) + n

--intentemos hacerla pero con listas 

sumatorialistas :: [Int]->Int
sumatorialistas a | a == [] = 0
                  | otherwise = head a + sumatorialistas ( tail a)

--ponemos el caso base a== [] = 0 porque sino por recurrencia el programa va a terminar haciendo el head y tail de [] que como vimos no tipa, asique cuando llegue a eso mejor que nos de 0

--longitud de una lista--
longitud :: [Int]->Int
longitud a | a == [] = 0
           | otherwise = 1 + longitud (tail a)


--si un num pertenece a la lista--
pertenece:: Int->[Int]->Bool
pertenece a b | a == head [] = False
              | (a == head b) || pertenece a (tail b)= True
              | otherwise = False

---hagamos el primermultiplode45345
primermultiplode45345 :: [Int]->Int
primermultiplode45345 a | mod (head a) 45345 == 0 = head a
                        | otherwise = primermultiplode45345 (tail a)

--hagamos la sumatoria usando pattern matching
sumatoriapm :: [Int]->Int
sumatoriapm [] = 0
sumatoriapm (a:ab)= a + sumatoriapm ab
--(a:ab) lo ponemos para definir cabeza y cola, entonces decimos que la sumatoria del head y el tail es igual a head (a) + sumatoriapm tail (ab)

--hagamos tmb la longitud en pattern matching

longitudpm :: [Int]-> Int
longitudpm [] = 0
longitudpm (a:ab) = 1 + longitud ab
--podriamos haber puesto tmb longitudpm (_:ab) ya que no importa lo q pongas en el head siempre va a valer 1








            
