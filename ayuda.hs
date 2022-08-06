
sonprimo n m | m >= n = True
             | mod n m == 0 = False
             | otherwise = sonprimo n (m+1)
    
esprimo:: Integer->Bool
esprimo 1 = False       
esprimo n = sonprimo n 2             
             


             
