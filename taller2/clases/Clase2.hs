f1 x y z = x**y + z <= x + y**z

f2 x y = (sqrt x)/(sqrt y)

f3 x y = div (sqrt x) (sqrt y)

f4 x y z | x == y = z
         | x**y == y = z
         | otherwise = z

f5 x y z | x == y = z
         | x**y == y = x
         | otherwise = y


cinco = 5

--k uplas                

suma2:: (Float , Float) -> (Float , Float) -> (Float , Float)
suma2 v w = ((fst v) + (fst w) , (snd v) + (snd w))

--en pattern matching quedaria--
suma (vx , vy) (wx , wy) = (vx + wx , vy + wy)

--esto devuelve el primer numero del vector
primersuma (vx , vy) (wx , wy) = fst (suma (vx , vy) (wx , wy))




