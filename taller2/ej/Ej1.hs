module Ej1   where
import Ej2 (prodInt , ortogonal)




absoluto::Integer -> Integer
absoluto a | a >= 0 = a
           | otherwise = -a
           
           
maxabs:: Integer -> Integer -> Integer
maxabs a b | absoluto a > absoluto b = a
           | absoluto b > absoluto a = b
           | otherwise = 0
           
           
max3:: Integer -> Integer -> Integer -> Integer
max3 a b c | a > b && a > c = a
           | b > a && b > c = b
           | otherwise = c
           
           
algunoEs0:: Integer -> Integer -> Bool
algunoEs0 a b | a == 0 = True
              | b == 0 = True
              | otherwise = False


--ahora con pattern matching
algunoEs00:: Integer -> Integer -> Bool
algunoEs00 a b = a== 0 || b == 0 


ambosSon0:: Integer -> Integer -> Bool
ambosSon0 a b | a == 0 && b == 0 = True
              | otherwise = False


ambosSon00:: Integer -> Integer -> Bool
ambosSon00 a b = a == 0 && b == 0


esmultiplode:: Integer ->Integer -> Bool
esmultiplode a b | mod b a == 0 = True
                 | otherwise= False
                
                
esmultiplodee:: Integer->Integer->Bool
esmultiplodee a b = mod b a == 0









