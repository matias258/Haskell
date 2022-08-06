--ejercicios de la clase 6

productoria :: [Int]->Int
productoria l | l == [] = 1
              | otherwise = (head l) * (productoria (tail l))
              
espar:: Int->Bool
espar n | mod n 2 == 0 = True
              
pares:: [Int]->[Int]
pares l p | espar (head l) =  
              
              
              
              
