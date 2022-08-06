module Ej2 where
--podria intentar importar a Ej1 pero si lo hago se formaria un ciclo, ya que Ej1 importa a Ej2

        




estanRelacionados:: Float->Float->Bool
estanRelacionados a b | b < 3 && a < 3 = True
                      | (3 <= b && 7 > b) && (3 <= a && a < 7) = True
                      | a > 7 && b > 7 = True
                      | otherwise = False


prodInt:: (Float, Float) -> (Float , Float) -> Float 
prodInt a b = ((fst a) * (fst b) + (snd a) * (snd b))


ortogonal:: (Float, Float) -> (Float , Float) -> Bool
ortogonal a b | prodInt a b == 0 = True
              | otherwise = False


todoMenor:: (Float, Float) -> (Float , Float) -> Bool
todoMenor a b | ((fst a) < (fst b)) && ((snd a) < (snd b)) = True
              | otherwise = False


distPuntos:: (Float, Float) -> (Float , Float) -> Float
distPuntos a b = sqrt((fst a - fst b)**2 + (snd a - snd b)**2)


sumaTerna:: (Int , Int , Int)->Int
sumaTerna (a , b , c) = a + b +c  


posicPrimerPar::(Int , Int , Int)->Int
posicPrimerPar (a , b , c) | mod a 2 == 0 = a
                           | mod b 2 == 0 = b
                           | mod c 2 == 0 = c
                           | otherwise = 4

crearPar::a -> b -> (a , b) 
crearPar a b = (a , b)


invertir:: (a , b) -> ( b , a)
invertir (a , b) = (b , a)










