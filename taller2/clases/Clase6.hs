--Listas--

sumatoria::[Int]->Int
sumatoria n | n == [] = 0
            | otherwise = sumatoria (tail n) + head n
            
longitud:: [Int]-> Int
longitud n | n == [] = 0
           | otherwise = 1 + longitud (tail n)            

pertenece :: Int -> [Int] -> Bool
pertenece n m | m == [] = False
              | n == head m = True
              | otherwise = pertenece n (tail m)

primermultiplode45345 :: [Int] -> Int
primermultiplode45345 n | n == [] = 0
                        | mod 45345 (head n) == 0 = head n
                        | otherwise = primermultiplode45345 (tail n) 


--escribamos sumatoria usando pattern matching

sumatoria2 :: [Int]-> Int
sumatoria2 [] = 0
sumatoria2 (x:xs)  = x + sumatoria2 xs

--longitud en PM

longitud2 :: [Int]-> Int
longitud2 [] = 0 
longitud2 (x:xs) = 1 + longitud2 xs

--pertenencia en PM

hayrepe:: Int -> [Int] -> Bool
hayrepe a xs | xs == [] = False
             | a == head (xs) = True
             | otherwise = hayrepe a (tail xs)
                
                 

hayrepetidos:: [Int] -> Bool
hayrepetidos xs | xs == [] = False
               | hayrepe (head xs) (tail xs) == True = True
               | otherwise = hayrepetidos (tail xs)
 






















































