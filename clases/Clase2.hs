--es la ecuacion de fibonacci
bact:: Int-> Int
bact n | n <= 1 = 1
       | otherwise = bact (n-1) + bact (n-2)

--ida es identidad de a , solo para n>= 0
ida:: Int->Int
ida 0 = 0
ida n = ida (n - 1) + 1

--idb es identidad de b , solo para n<= 0
idb :: Int-> Int
idb 0 = 0
idb n = (-1) + idb (n+1)

f 0 = 0
f _ = undefined
-- para todo f diferente de 0 , tirar indefinido

espar:: Integer -> Bool
espar n | mod n 2 == 0 = True
        | otherwise = False

espar1:: Int -> Bool
espar1 0 = True
espar1 1 = False
espar1 n = espar1 (n - 2)


espar2:: Int-> Bool
espar2 0 = True
espar2 1 = False
espar2 n = espar2 (n+2) && espar2 (n-2)

unoadelantetresatras:: Integer-> Integer
unoadelantetresatras 0 = 0
unoadelantetresatras 1 = 1
unoadelantetresatras n | espar n = 1 + unoadelantetresatras (n+1)
                       | otherwise = 1 + unoadelantetresatras (n-3)

--cantidad de digitos
cantdigitos:: Int-> Int                  
cantdigitos 0 = 0
cantdigitos n = cantdigitos (div n 10) + 1

--decimal interpretado como binario
esbinario n = (n == 0) || esbinario (div n 10) && (mod n 10) <= 1

--ejemplo de f recursiva con 2 parametros
dosparams:: Int -> Int -> Int
dosparams 0 _ = 0
dosparams _ 0 = 0
dosparams m n | mod m 2 == 0 = n + dosparams ( m - 2) (n - 1)
              | otherwise = n + dosparams (m + 2) (n - 1)
-- is m es par -> (m - 2 , n + 1)
--sino (m + 2 , n - 1)













