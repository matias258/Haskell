

tribonacci::Int->Int
tribonacci n | n <= 2 = n
             | otherwise = tribonacci (n - 1) + tribonacci (n-2) + tribonacci (n-3)
             
multipde3:: Int->Bool
multipde3 n | n<3 = False
            | otherwise =(n==3) || (multipde3(n-3))



diabolico::Int ->Bool
diabolico n | n/=6 = False
            | n == 6 = True
            | mod n 10 == 6 = diabolico ( div n 10)

iguales :: Int->Bool
iguales n | n == 0 = True
          | n <= 10 = True
          | mod n 10 == div n 10 = True
          | mod n 10 == div (mod n 100) 10 && iguales (div n 10) = True
          | otherwise= False


potenciadeotro:: Int-> Int-> Bool
potenciadeotro n m = mod m n == 0 || mod n m == 0





