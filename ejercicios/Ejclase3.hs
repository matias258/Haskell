sumatoria::Int->Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

f1::Int->Int
f1 0 = 1
f1 n = 2^n + f1(n-1)

f2:: Int->Float->Float
f2 0 q = 0
f2 n q = q^n + f2 (n-1) q

f3:: Int->Float->Float
f3 0 q = 0
f3 n q = q^(2*n -1) + q^(2*n)+ f3(n-1) q

f4::Int->Float->Float
f4 0 q = 1
f4 n q = f4(n-1) q + q^(2*n-1) + q^(2*n) - q^(n-1)

fcuatro::Int->Float->Float
fcuatro 0 q = 0
fcuatro n q = f3 n q - f2(n-1) q 


fact 1 = 1
fact n = n* fact(n-1)

eaprox::Integer->Float
eaprox 0 = 1
eaprox n = 1/(fromIntegral(fact n)) + eaprox(n-1)

e::Float
e= eaprox 10

--sumatorias dobles

f:: Int->Int->Int
f 0 m = 0
f n 0 = 0
f n m = f(n-1) m + round(f2 m (fromIntegral n))


efe::Int->Int->Int
efe 0 m = 0
efe n 0 = 0
efe n m = efe n (m-1) + round(f2 n (fromIntegral m))


sumapotencias:: Int-> Int->Int->Int
sumapotencias 0 n m = 0
sumapotencias q 0 0 = q
sumapotencias q n m = sumapotencias q (n-1) m + q^(n+ (m-1)) + q^(n + m)


sumarac ::Int->Int->Float
sumarac 0 m = 0
sumarac n 0 = 0
sumarac n m = sumarac n (m-1) + (fromIntegral (sumatoria n))/(fromIntegral m)

--pasemos a los ej posta xdd

g1::Float->Int->Float
g1 0 n = 0
g1 i 0 = 1
g1 i n = g1 (i-1) n + f2 n i

aux::Int->Int->Int 
aux n 0 = 0
aux n m = m^n + aux n (m-1)

g2:: Int->Int
g2 0 = 0
g2 n = g2 (n-1) + aux n n


g3::Int->Int
g3 0 = 0
g3 n = g3 (n-2) + 2^n
















