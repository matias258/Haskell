
--auxiliar
menorfactdesdedesde :: Int->Int->Int
menorfactdesdedesde i m | factorial i >= m = factorial i
                        | otherwise = menorfactdesdedesde (i+1) m
--minimo k! tal que k! >= m y k >= i

--aux del auxiliar
factorial:: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = factorial (n - 1) * n 


--entonces menorfactdesde nos quedaria
menorfactdesde :: Int-> Int
menorfactdesde m = menorfactdesdedesde 1 m



mayorfacthastahasta::Int->Int->Int
mayorfacthastahasta m k | factorial k > m = factorial (k-1)
                        | otherwise = mayorfacthastahasta m (k+1)


mayorfacthasta :: Int->Int
mayorfacthasta m = mayorfacthastahasta m 1




sonfact :: Int-> Int-> Bool
sonfact a b | b == 0 = False
            | factorial b == a = True
            | factorial b /= a = sonfact a (b-1)
            

esfact:: Int-> Bool
esfact m = sonfact m m



fibonacci:: Int-> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibo :: Int-> Int->Bool
fibo a b | a == fibonacci b = True
         | fibonacci b > a = False
         | a /= fibonacci b = fibo a (b+1)
         

esfibo :: Int-> Bool
esfibo n = fibo n 1
        
-- auxiliar

sumadivhasta :: Int->Int->Int
sumadivhasta n k | k == 1 = 1
                 | mod n k == 0 = k + sumadivhasta n (k-1)
                 | otherwise = sumadivhasta n (k-1)

sumadivisores :: Int-> Int
sumadivisores n = sumadivhasta n n
---------

tomavalormax:: Int -> Int -> Int
tomavalormax n1 n2 | n1 > n2 = 0
                   | n1 < 1 = 0
                   | n1 == n2 = n2
                   | sumadivisores n1 > sumadivisores (tomavalormax (n1 +1) n2) = n1
                   | otherwise = tomavalormax (n1 + 1) n2







