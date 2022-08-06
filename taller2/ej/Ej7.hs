type Set a = [a]

--auxiliar--------------------------------------------
--auxiliar-------------------------------------------------

vacio :: Set Int
vacio = []

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs:xss

agregaratodos :: Int -> Set (Set Int) -> Set (Set Int)
agregaratodos x [] = []
agregaratodos x (c:cs) = agregarC (agregar x c) (agregaratodos x cs)   

unionC:: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC a b | a == [[]] = b
           | otherwise = a ++ b             
--------------------------------------------------------

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregaratodos x (partes xs))

---------------------------------------------------------
















