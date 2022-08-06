module Ejclase2
       where

related::Float -> Float -> Bool
related x y | x < 3 && y <3 = True
                      | 3<=x && x<7 && 3<=y && y<7 = True
                      | x>7 && y>7 =True
                      | otherwise= False


prodinterno:: (Float, Float) -> (Float, Float) -> (Float , Float)
prodinterno x y = ((fst x * fst y) , (snd x * snd y)) 

todomenor:: (Float, Float) -> (Float, Float) -> Bool
todomenor x y | (fst x) < (fst y) && (snd x) < (snd y) = True
              | otherwise= False
              
distpts::(Float, Float) -> (Float, Float) -> Float
distpts x y = sqrt(((fst x + fst y)**2) + ((snd x + snd y)**2))

sumaterna:: (Int , Int , Int) -> Int
sumaterna (x1 , x2 , x3) = x1 + x2 + x3

posicion1erpar::(Int , Int , Int) -> Int
posicion1erpar (x1 , x2 , x3) |  espar x1 == True = x1
                              |  espar x2 == True = x2
                              |  espar x3 == True = x3
                              | otherwise = 4
                              where espar b | mod b 2==0=True
                                            |otherwise = False
                              

crearpar:: a -> b -> (a , b)
crearpar a b = (a , b)

invertir:: (a , b) -> (b , a)
invertir (a , b)= (b , a)






