f x y = x * x + y * y

g x y z = x + y + z * z

doble x = x * 2
suma x y = x + y
normavectorial x y = sqrt(x^2 + y^2)
funcionconstante8 x = 8

h n | n == 0 = 1
    | n /= 0 = 0


signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

maximo x y | x >= y = x
           | otherwise = y

f2 n| n >= 3 = 5
    | n == 2 = undefined 
    | otherwise = 8


--Pattern matching--

f5 n | n == 0 = 1
    | n /= 0 = 0
--tmb se puede hacer (usando pm) como:

f6 0 = 1
f6 n = 0


signo2 0 = 0
signo2 n | n > 0 = 1
         | otherwise = -1
         

cantidaddesol b c | b^2-4*c > 0 = 2
                  |  b^2-4*c == 0 = 1
                  | otherwise = 0

--otra forma

cantidaddesol2 b c | d > 0 = 2
                   | d == 0 = 1
                   | otherwise = 0
                   where d = b^2 - 4*c













