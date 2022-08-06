
type Set a = [a]
--es para que cuando haces :t de vacio t devuelva set de int, osea lista de ints

vacio:: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece n m | m == [] = False
              | n == head m = True
              | otherwise = pertenece n (tail m)

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x : c


incluido :: Set Int -> Set Int -> Bool
incluido a b | a == [] = True
             | pertenece (head a) b == True = pertenece (head (tail a)) b
             | otherwise = False

--otra forma

incluido2 :: Set Int -> Set Int -> Bool
incluido2 [] c = True
incluido2 (x:xs) c = pertenece x c && incluido2 xs c


--auxiliar
longitud:: [Int]-> Int
longitud n | n == [] = 0
           | otherwise = 1 + longitud (tail n)
---

iguales :: Set Int -> Set Int -> Bool
iguales a b = longitud a == longitud b && incluido a b

--otra forma

iguales2 :: Set Int -> Set Int -> Bool
iguales2 c1 c2 = incluido c1 c2 && incluido c2 c1


union :: Set Int -> Set Int -> Set Int
union a b | a == [] = b
          | b == [] = a
          | otherwise = a ++ b



interseccion:: Set Int -> Set Int -> Set Int
interseccion a b | a == [] || b == [] = []
          | pertenece (head a) b == True = agregar (head a) (interseccion (tail a) b)
          | pertenece (head a) b == False = interseccion (tail a) b


--auxiliar-------------------------------------------------
perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs:xss

agregaratodos :: Int -> Set (Set Int) -> Set (Set Int)
agregaratodos x [] = []
agregaratodos x (c:cs) = agregarC (agregar x c) (agregaratodos x cs)   

unionC:: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC a b | a == [[]] = b
           | otherwise = a ++ b             
--------------------------------------------------------

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregaratodos x (partes xs))



-----------------------------------
partesN :: Int -> Set (Set Int)
partesN a | a == 0 = []
          | a /= 0 = partes ([1..a])




--auxiliar
producto:: Int -> Int -> (Int , Int)
producto a b | a == 0 = (0 , b)
             | b == 0 = (a , 0)
             | otherwise = (a , b) 
 

quitar :: Int -> [Int] -> [Int]
quitar a b | b == [] = []
           | head b == a = tail b
           | head b /= a = (head b) : quitar a (tail b) 
-------------------------------
            
productcart:: Set Int-> Set Int -> Set (Int , Int)
productcart a b = [producto (head a) b] + productcart (quitar (head a) a) b






