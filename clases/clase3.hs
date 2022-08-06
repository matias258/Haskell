--SUMATORIAS---------------

sumatoria::Int->Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)


f1::Int->Int
f1 0 = 1
f1 n = (f1 (n-1)) + 2^n


f2::Int-> Float->Float
f2 0 q = 0
f2 n q = q^n + f2(n-1) q
--o tmb y mas facil--

fdos::Int->Float->Float
fdos n q | n== 0 = 0
         | otherwise = q^n + f2 (n-1) q
         

f3::Int->Float->Float
f3 0 q = 0
f3 n q = (f3(n-1) q) + q^(2*n-1) + q^(2*n)


f4::Int->Float->Float
f4 0 q = 1
f4 n q = q^(2*n-1) + q^(2*n) -q^(n-1) + (f4(n-1) q)

--o sino uso f3
--como f3 tiene un rango de 1 a 2n, tengo q restarle de 1 a n,para eso uso f2 q tiene rango 1 a n

fcuatro:: Int->Float->Float
fcuatro n q | n==0 = 1
            | otherwise = (f3 n q) - (f2 (n-1) q)


fact::Int->Int
fact 1 = 1
fact n = n * (fact (n-1))

eaprox::Int->Float
eaprox 0 = 1
eaprox n = eaprox(n-1) + (1/fromIntegral(fact n))
--la division espera dos float pero le estamos dando 2 int, fromintegral hace q un num entero pase a Float asi no tira error

e::Float
e=eaprox 10
--hicimos la constante e


--sumatorias dobles
f::Int->Int->Int
f n m | n == 0 = 0
      | otherwise = (f(n-1) m) + round(f2 m (fromIntegral n))
--como en f2 tenemos Int->Float->Float , f no funcionaba xq al poner a f2 dentro d la ecuacion, este ultimo buscaba un Float. X eso pusimos el fromIntegral. El 2do problema vino con que Haskell no sabe sumar un float con un int, entonces le ponemos round and float para redondearlo a int y q pueda maniobrar.

sumapotencias::Float->Int->Int->Float
sumapotencias q n 0=0
sumapotencias q n m = (sumapotencias q n (m-1)) + q^m*(f2 n q) 



sumaracionales::Int->Int->Float
sumaracionales n 0 = 0
sumaracionales n m = (sumaracionales n (m-1)) + (fromIntegral(sumatoria n)) / (fromIntegral m) 
--mismo caso con el anterior, debemos pasar con el fromintegral para q divida dos Float



