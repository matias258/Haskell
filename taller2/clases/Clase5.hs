prod d h | h == d = d
         | otherwise = prod d (h-1) * h
--aca h decrece hasta el caso base, lleganod tambien a d

prod2 d h | d == h = d
          | otherwise = prod2 (d+1) h * d
-- en este ningun parametro decrece, sino q d crece hasta h, se acerca hasta el caso base

sumadivhasta :: Int->Int->Int
sumadivhasta n k | k == 1 = 1
                 | mod n k == 0 = k + sumadivhasta n (k-1)
                 | otherwise = sumadivhasta n (k-1)
--suma numeros en el rango 1 a k tal que sean divisores de n

--ya hicimos el caso mas complejo, para resolver la funcion sumadivisores, que nos quedaria:

sumadivisores :: Int-> Int
sumadivisores n = sumadivhasta n n

--otra forma era creando sumadivdesde
sumaDivdesde :: Int->Int->Int
sumaDivdesde n k | k == n = n
                 | mod n k == 0 = k + sumaDivdesde n (k+1)
                 | otherwise = sumaDivdesde n (k+1)

sumadiv2 :: Int->Int
sumadiv2 n = sumaDivdesde n 1
--y asi nos quedaria sumadiv con sumadivdesde



menordivdesde:: Int->Int->Int
menordivdesde n k | k == n = n
                  | mod n k == 0 = k
                  | otherwise = menordivdesde n (k+1) 
--grax a esto podemos hacer menordiv que sea mayor que 1:
menordiv:: Int->Int
menordiv n = menordivdesde n 2



esprimo :: Int -> Bool
esprimo n | sumadivisores n - 1 == n = True
          | otherwise = False
--nada q ver con el ej, pero me acabo d inventar un programita q me devuelve si es primo o no, ya que la suma de todos los divisorres de un primo son el mismo primo y 1 tonces: n = sumadivisores n -1

esprimo2 :: Int->Bool
esprimo2 n | menordiv n == n = True
           | otherwise = False


nesimoprimo::Int->Int
nesimoprimo 1 = 2
nesimoprimo n  = minimoprimodesde (n+1)

--auxiliar 
minimoprimodesde :: Int->Int
minimoprimodesde n | esprimo n = n
                   | otherwise = minimoprimodesde (n+1)





