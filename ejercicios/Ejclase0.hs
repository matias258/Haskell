module Ejclase1
where


absoluto:: Integer -> Integer
absoluto n | n <= (-1) = (-n)
           | n >= 1 = n
           | otherwise = 0

maxabs:: Integer -> Integer -> Integer
maxabs a b | absoluto a >= absoluto b = absoluto a
           | otherwise = absoluto b

max3:: Integer->Integer->Integer-> Integer
max3 a b c | a >= b && a >= c = a
           | b >= a && b >= c = b
           | c >= a && c >= b = c

algunoes0:: Integer -> Integer-> String
algunoes0 a b | a==0 && b==0 = "A y B son 0"
              | b == 0 = "B es 0"
              | a == 0 = "A es 0"
              | otherwise = "Ninguno es 0"

lgunoes0::Integer->Integer->String
lgunoes0 a b | a==0 || b==0 = "Alguno es 0"

ambosson0:: Integer-> Integer-> String
ambosson0 a b | a==0 && b==0 = "Ambos son 0"
              | otherwise = "Al menos uno no es 0"

esmultde:: Int-> Int-> String
esmultde a b | a>=b && mod a b ==0 ="a es multiplo de b"
             | b>= a && mod b a == 0 = "b es multiplo de a"
             | otherwise = "No son multiplos"

digitounidades:: Int-> Int
digitounidades a = mod a 10  

digitodecenas:: Int-> Int
digitodecenas a = mod a 100          















           

