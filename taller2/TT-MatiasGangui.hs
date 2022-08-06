
-- Ej 1 ----------------------------------------------------------------------------------------------------------------------------

med :: Float -> Float -> Int -> Float
med i0 b n | n == 0 = i0
           | otherwise = med infec b (n-1)
           where infec = i0 + i0 * b
-- i0 = cantidad de infectados inicial.
-- b = tasa de infeccion.
-- n = cantidad de dias.

--Copie la ecuacion usando recurrencia en n hasta que llegue a n = 0 , donde ahi haskell daria el valor de i0: "n == 0 = i0".
--Defini infec aparte, para que no tenga que calcularlo desde el comienzo cada vez que hace recurrencia

-- Ej 2 ------------------------------------------------------------------------------------------------------------------------------

mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n | n == 0 = i0
             | otherwise = mld p infec b (n-1)
             where infec = i0 + i0 * b * ((p-i0) / p)
                   sanos = p - infec
                   
                   
-- Casos Bases: igual que el 1), si n == 0 esto nos da el valor de i0 , ya que nos pide definir la base.
-- Uso el where para que haskell guarde los valores ya calculados y no tenga que volver a calcularlos
-- Despues, copie la nueva ecuacion usando recurencia en n.



-- Ej 3 --------------------------------------------------------------------------------------------------------------------------
--auxiliar que me calcula el maximo entre 2 Floats


----------------------------------------------------------------------------          



sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0 , i0 , r0) b g n | n == 0 = (s0 , i0 , r0)
                         | otherwise =sir (sanos , infec , recup) b g (n-1) 
                          where recup = r0 + g * i0
                                sanos = s0 - b * i0 * s0
                                infec = i0 + b * i0 * s0 - g * i0
                                
                                
-- Utilizo el where para definir infec recup y sanos, y me ayudo de la recurrencia en n (los dias)                             
                                

sir2 :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir2 (s0 , i0 , r0) b g 0 = (s0 , i0 , r0)
sir2 (s0 , i0 , r0) b g n = (s_ant - b * i_ant * s_ant , i_ant + b*i_ant * s_ant - g * i_ant , r_ant + g * i_ant)
                          where (s_ant , i_ant, r_ant) = sir2 (s0 , i0 , r0) b g (n-1)
                          
                          
          
            
            
--Ej 4 ------------------------------------------------------------------------------------------------------------------------------------------

maxim :: Float -> Float -> Float
maxim n m | n < m = m
          | n >= m = n
-- auxiliar --
          
          
maxsir :: (Float , Float , Float) -> Float -> Float -> Int -> Float
maxsir (s0 , i0 , r0) b g n | n == 0 = i0
                            | otherwise = maxim (maxsir (sanos , infec , r0) b g (n-1)) (maxsir (s0 , i0 , r0) b g (n-1))
                            where sanos = s0 - b * i0 * s0
                                  infec = i0 + b * i0 * s0 - g * i0
                            

--defini maxim, que toma el valor max entre 2 Floats. Y me ayudo de este para buscar el maximo de maxsir luego de "n" dias, tambien utilizo el where otra vez para definir los valores que me interesan (en este caso infec y sanos) 





                                  


--Ej5--------------------------------------------------------
mejora1:: (Float , Float , Float) -> Float -> Float -> Int -> Bool
mejora1 (s0 , i0 , r0) b g n | n == 0 = False
                             | recup > infec = True
                             | otherwise = mejora1 (sanos , infec , recup) b g (n-1)
                             where sanos = s0 -b * i0 *s0
                                   infec = i0 + b* i0 * s0 - g* i0
                                   recup = r0 + g * i0

                                  
                                  
--Use el where para defininir recup, sanos e infec. Si recup > infec para algun dia entre 1 a n, mejora va a ser True (recup > infec = True). Si no, hacemos recursion en n hasta llegar al 0, donde (si no encontramos nunca que recup > infec) concluiremos que es False.


