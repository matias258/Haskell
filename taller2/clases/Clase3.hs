module Clase3
where 


factorial:: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = factorial (n - 1) * n 
            | n < 0 = factorial (n + 1) * n

esPar:: Int-> Bool
esPar n | n == 1 = False
        | n == 0 = True
        | otherwise = esPar (n-2)
        

fib:: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)


parteEntera::Float ->Integer
parteEntera n | n < 0 = undefined
              | n < 1 = 0
              | n > 0 = 1 + parteEntera ( n - 1) 










