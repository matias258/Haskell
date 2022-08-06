module clase2
where

identidad x = x
--aca el tipo es de p1->p1
--acepta todos los tipos, ej booleano int etc y los devuelve

segundo x y = y
--el tipo es p1->p2->p2 
--ya q tenemos 2 incognitas ( de cualquier tipo) y devuelve el 2do valor

triple x = 3*x
--aca el tipo es Num a -> a -> a
--Osea q solo permite nuumeros (No Bool)

maximo x y | x >= y = x
           | otherwise =y
--este va a hacer Ord p -> p-> p-> p
--Ord significa que sus elementos sean comparables entre ellos

distintos x y = x /= y
--Eq a -> a -> a -> Bool
--Eq: Que sean comparables x igualdad o por distinto

f4 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z

--TUPLAS--------------------------------------
suma::(Float, Float)->(Float, Float)->(Float, Float)
suma v w = ((fst v)+ (fst w), (snd v) + (snd w))

normavect1::(Float, Float)->Float
normavect1 (x , y) = sqrt((x**2)+ (y**2))

sumavect1::(Float, Float)->(Float, Float)->Float
sumavect1 v w= normavect1 ((fst s), (snd s))
     where s= suma v w





         
         
