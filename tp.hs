
sonprimo n m | m >= n = True
             | mod n m == 0 = False
             | otherwise = sonprimo n (m+1)
    
esprimo:: Integer->Bool
esprimo 1 = False       
esprimo n = sonprimo n 2                    

soncoprimos::Integer->Integer->Bool
soncoprimos n m | mod (m^(n-1)-1) n == 0 && esprimo n==True && esprimo m== True = True
                | otherwise = False

es2pseudoprimo::Integer->Bool
es2pseudoprimo n | mod (2^(n-1) -1) n == 0 && esprimo n == True = True
                 | otherwise = False























                 
