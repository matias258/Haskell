module Naturales
where 
      import Prelude ( Int, Bool(True , False) , succ , pred , ( || ) , ( && ) , not , otherwise)
-- succ= sucesor y pred= predecesor, ej pred 5 = 4 y succ 4 = 5    
      suma:: Int -> Int -> Int
      suma 0 n = n      
      suma m n = suma (pred m) (succ n)
      
      mult:: Int->Int->Int
      --m * n = (1 + ( m - 1)) * n = n + (m - 1) * n
      mult 0 n = 0
      mult m n = suma n (mult(pred m)n)
      
      restar:: Int->Int->Int
      restar n 0 = n
      restar n m = restar (pred n)(pred m)
            
                  
      menor::Int->Int-> Bool     
      menor m n = menor (pred m)(pred n)
      menor (( pred m)-n) 0= True 
      menor 0 n = False  
      menor 0 0 = False   
      
                
                     
      
      
      
      
   
           
