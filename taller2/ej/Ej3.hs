module Ej3 where

       


multiplo::Int->Bool
multiplo n | n <= 0 = False
           | 3 * n == 9 = True
           | otherwise =  multiplo(n-3) 
--Lo que hice fue decir, bueno tengo un numero, si es multiplo de 3 lo hago restar multiplo(n-3) hasta que llegue a 3, y si cumple 3 * n = 9 es mult, sino llegua a n<=0 y no es multiplo

impar :: Int-> Int
impar n | mod n 2 == 1 = 1
        | otherwise = 0

sumaImpares:: Int->Int
sumaImpares n | n == 0 = 0
              | otherwise = 2*n - 1 + sumaImpares (n-1)
--serie de i=1 hasta n de 2i-1 (que solo da impares)

factorial:: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = factorial (n - 1) * n 
            | n < 0 = factorial (n + 1) * n

medioFact ::Int -> Int
medioFact n | n <= 1 = 1
            | impar n == 1 = n * medioFact(n-2)
            | impar n == 0 = n * medioFact (n-2)
--multiplica los numeros pares o impares dependiendo del num dado hasta el 1, ej n= 10 => 10*8*6*4*2=3840


suma:: Int->Int->Int
suma n m | n == 0 = m
         | m == 0 = n
         | m <= 0 || n <= 0 = undefined
         | otherwise = 1 + suma n (m-1)

igual::Int ->Bool
igual n | n <= 0 = False
        | mod n 10 *  


digitos::
digitos n | n < 10 = 1
          | otherwise 1 + digitos ( div n 10)



















