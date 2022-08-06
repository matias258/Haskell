-- ej de listas

productoria :: [Int]-> Int
productoria n | n == [] = 1
              | otherwise = (head n) * productoria (tail n)


sumarN:: Int-> [Int] -> [Int]
sumarN n xs | n == 0 = xs
            | xs == [] = xs
            | otherwise = (n + head xs) : sumarN n (tail xs)


sumarelprimero :: [Int] -> [Int]
sumarelprimero xs | xs == [] = xs
                  | otherwise = sumarN (head xs) xs



sumarelultimo :: [Int] -> [Int]
sumarelultimo xs | xs == [] = xs
                 | otherwise = sumarN (last xs) xs


--pares no salio


quitar :: Int -> [Int] -> [Int]
quitar a b | b == [] = []
           | head b == a = tail b
           | head b /= a = (head b) : quitar a (tail b) 


quitartodas:: Int -> [Int] -> [Int]
quitartodas a b | b == [] = []
                | head b == a = quitar a (tail b)
                | head b /= a = (head b) : quitartodas a (tail b)


-- hayrepe no salio, entonces tampoco los consiguientes

--auxiliar 
longitud2 :: [Int]-> Int
longitud2 [] = 0 
longitud2 (x:xs) = 1 + longitud2 xs
--

maximo:: [Int] -> Int
maximo (x:y:xs) | xs == [] && x > y = x
                | xs == [] && x < y = y
                | x < y = maximo (y:xs)
                | x > y = maximo (x:xs)

--auxiliar

minimo :: [Int] -> Int
minimo (x:y:xs) | xs == [] && x > y = y
                | xs == [] && x < y = x
                | x < y = minimo (x:xs)
                | x > y = minimo (y:xs)

               

                 


