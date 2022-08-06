--productoria de un D a un H
prod::Int->Int->Int
prod d h | d == h = d
         | d > h = 0
         | otherwise = h * prod d (h-1)
--vemos que adentro tenemos el factorial, vemos q si hacemos d=1 y h=n es el fact de 1 a n

prod2::Int->Int->Int
prod2 d h | d == h = d
          | d > h = 0
          | otherwise = d * prod2 (d+1) h
         
sumadivhasta::Int->Int->Int
sumadivhasta n k | k == 1 = 1
                 | mod n k == 0 = k + sumadivhasta n (k-1)
                 | otherwise = sumadivhasta n (k-1)
                 
--con esto puedo hacer el sumadivisores
sumadivisores::Int->Int
sumadivisores n = sumadivhasta n n
--entonces suma todos los nums dividiendo al n dado, ej sumadiv 6 = 1+2+3+6
                
menordivdesde:: Int->Int->Int
menordivdesde n k | mod k n == 0 = k
                  | otherwise = menordivdesde n ( k+1)
 
menordivisor:: Int->Int
menordivisor n = menordivdesde n 2 
 
--busquemos el menorfactdesde
--para eso buscamos antes el menorfactdesdedesde i m , que busca el minimo k! tal que k!>= m y k! >= i
fact::Int->Int
fact 1 = 1
fact n = n * (fact (n-1))

menorfactdesdedesde::Int->Int->Int
menorfactdesdedesde i n | (fact i ) == n = fact i
                        | otherwise = menorfactdesdedesde (i + 1) n

menorfactdesde:: Int->Int
menorfactdesde n = menorfactdesdedesde 1 n









                 
