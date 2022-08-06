module clase01
where

f x y = x * x + y * y

g x y z = x + y + z * z

doble x = x * 2

suma var1 var2 = var1 + var2

normavect x1 x2 = sqrt ( (x1)^2 + (x2)^2)

signo n | n > 0 =1
        | n == 0 = 0
        | otherwise = -1
        
maximo x y | x >= y = x
           | x == y = x
           | x <= y = y

-- cantidad de soluciones en una cuadratica --

cantsolu b c | d == 0 = print ("1 solucion posible")
             | d <= (-1) = print ("No existen soluciones reales")
             | d >=1 = print ("Existen 2 soluciones posibles")
             where d = b^2 - 4*c
             
espar :: Int -> Bool
espar n | mod n 2 == 0 = True
        | otherwise = False

esimpar n = not (espar n)
             
funcionrara :: Float -> Float -> Bool -> Bool
funcionrara x y z = ( x >=y ) || z







