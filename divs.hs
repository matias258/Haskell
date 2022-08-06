--Empezamos utilizando la definicion de esprimo vista en la clase5

menordiv::Integer->Integer
menordiv n = menordivdesde n 2

menordivdesde::Integer->Integer->Integer
menordivdesde n k | mod n k == 0 = k
                  | otherwise = menordivdesde n (k+1)

mayordivdesde:: Integer->Integer->Integer
mayordivdesde n k | mod n k == 0 = k
                  | otherwise = mayordivdesde n (k-1)                  
                  
esprimo ::Integer->Bool
esprimo 1 = False
esprimo n = n == menordiv n 
          
--- ahora definamos el maximo comun divisor (mcd)

mcd :: Integer -> Integer ->Integer
mcd a b | 
