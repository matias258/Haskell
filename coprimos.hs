--Empezamos utilizando la definicion de esprimo vista en la clase5

menordiv::Integer->Integer
menordiv n = menordivdesde n 2

menordivdesde::Integer->Integer->Integer
menordivdesde n k | mod n k == 0 = k
                  | otherwise = menordivdesde n (k+1)
                  
                  
esprimo ::Integer->Bool
esprimo 1 = False
esprimo n = n == menordiv n 
        
        
compuestos::Integer->Bool
compuestos n = not (esprimo n)        
          
--- ahora definamos el maximo comun divisor (mcd)

mcd :: Integer ->Integer->Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

--Podemos proseguir con los ejercicios

--Definamos sonCoprimos

sonCoprimos:: Integer ->Integer->Bool
sonCoprimos a b | mcd a b == 1 = True
                | otherwise = False
                
--Calculamos es2Pseudoprimo                
                
es2Pseudoprimo::Integer->Bool
es2Pseudoprimo n | n == 1 = False
                 | compuestos n && mod (2^(n-1)-1) n == 0 = True
                 | otherwise = False                
                 
--Calculamos cantidad3Pseudoprimo, para eso tomamos es3Pseudoprimo como auxiliar
                 
es3Pseudoprimo::Integer->Bool
es3Pseudoprimo n | n == 1 = False
                 | compuestos n && mod (3^(n-1)-1) n == 0 = True
                 | otherwise = False                 
                 
cantidad3Pseudoprimos::Integer->Integer
cantidad3Pseudoprimos m | m == 1 = 0 
                        | es3Pseudoprimo m == True = cantidad3Pseudoprimos (m-1) + 1
                        | es3Pseudoprimo m == False = cantidad3Pseudoprimos (m-1) + 0                 
                 
                 
simultaneo2y3::Integer->Integer
simultaneo2y3 a | es3Pseudoprimo a == True && es2Pseudoprimo a == True = 
                | a == 1 = 0
                | otherwise = simultaneo2y3 (a-1)                
                 
                 
                 
                 
                 
                 
