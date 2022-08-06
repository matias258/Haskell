sumadivhasta::Int->Int->Int
sumadivhasta i n | i == n = i
                 | mod n i == 0 = sumadivhasta (i+1) n + i
                 | otherwise = sumadivhasta (i+1) n 

sumadivisores::Int->Int
sumadivisores n = sumadivhasta 1 n



menordivdesde :: Int -> Int -> Int
menordivdesde i n  | mod n i >= 1 = menordivdesde (i+1) n
                   | mod n i == 0 = i
                   
menordivisor::Int->Int
menordivisor n = menordivdesde 2 n                   
                   
                   
esprimo::Int->Bool
esprimo n | menordivisor n == n = True
          | otherwise = False  
          | n == 1 = False                
                   
nesimoprimo::Int->Int
nesimoprimo n | esprimo n == True = n
              | otherwise = menordivisor n                 
       
fibonacci::Int->Int->Int
fibonacci i n | n == 0 = 0
              | n == 1 = 1
              | n > i = fibonacci i (n-1) + fibonacci i (n-2)


primerosprimos::Int->Int
primerosprimos m | mod m 2 == 1 = mod (m-1) 2
                 | mod m 2 == 0 = m + mod (m-1) 2 
                 | m == 0 = 0





                   
                   
                   
                   
