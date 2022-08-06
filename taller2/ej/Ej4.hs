--ayudas--------------------
factorial:: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = factorial (n - 1) * n 
            
----------------------------


f1 :: Int-> Int
f1 n | n == 0 = 1
     | n < 0 = undefined
     | otherwise = 2^n + f1 (n-1)


f2 :: Int -> Int -> Int
f2 m n | m == 0 = 0
       | n == 1 = m
       | otherwise = m^n + f2 m (n-1)


f3 m n | m == 0 = 0
       | n <= 0 = 0
       | n > 0 = m^(2*n-1) + m^(2*n) + f3 m (n-1)


f4 m n | m == 0 = 0
       | n <= 0 = 0
       | otherwise = m^n + (f3 m n) - (f2 m n)
--escribir en papel la sumatoria y que abarca cada f

eAproxim :: Integer -> Float 
eAproxim 0 = 1
eAproxim n = (eAproxim (n-1)) + 1 / fromIntegral(factorial n)
--fromIntegral transforma Int en Float         
-- round redondea un num Float o Integer y me lo redondea a Int

e::Float
e = eAproxim 10

efe:: Int -> Int -> Int
efe m n | n == 0 = 0
        | otherwise = efe n (m-1) + f2 m n



sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q n m | n == 1 = q^(m+1)
                    | m == 1 = q^(n+1)
                    | otherwise = (sumaPotencias q n (m-1)) + (f2 q n)*q^m

--Auxiliar------------------
sumaAux:: Float -> Float -> Float
sumaAux n m | n == 1 = 1 / m
            | otherwise = sumaAux (n-1) m + n / m
----------------------------

sumaRacionales:: Float -> Float -> Float
sumaRacionales n m | m < 1 = 0
                   | otherwise = sumaAux n m + sumaRacionales n (m-1)

----------------------ejercicios posta------


g1:: Integer -> Integer -> Integer
g1 i n | n == i = i^n
       | otherwise = g1 i (n-1) + i^n
--lo q pensamos aca es, mi j esta definida / j==i, asique no es una variable independiente. x eso g1 depende de i y de n, i define a j y a la funcion dentor de la sumatoria y n define el max rango de la sumatoria
-- si hacemos i = 2 y n = 5 -> suma de 2 a 5 de 2^i = 2^2 + 2^3 + 2^4 + 2^5 = 60


g2::Int -> Int
g2 n | n == 0 = 0
     | otherwise = g2 (n-1) 











