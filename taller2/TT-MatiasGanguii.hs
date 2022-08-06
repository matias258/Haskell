
--tp2

--ej1

--a)

--auxiliar----------------------------------------
divisores:: Int-> Int -> [Int]
divisores a b | a == 0 = []
              | b <= 0 = []
              | mod a b == 0 = [b] ++ divisores a (b-1)
              | otherwise = divisores a (b-1)
            
divi :: Int -> [Int]
divi a = divisores a (a-1)


sumatoria2 :: [Int]-> Int
sumatoria2 [] = 0
sumatoria2 (x:xs)  = x + sumatoria2 xs

-----------------------------------------------------------

sumaDeDivisoresPropios :: Int -> Int
sumaDeDivisoresPropios a | a == 0 = 0
                         | otherwise = sumatoria2 (divi a) 

--Me ayude de 3 auxiliares, divisores = me da la cantidad de divisores de un numero hasta un "b" dado; divi = es divisores de "a" hasta el numero anterior a "a" (para que no aparezca el mismo numero dividiendo); sumatoria2, la saque de la clase 6, calcula la suma de los numeros de una lista.

--b)--------------

esPerfecto:: Int-> Bool
esPerfecto a | a <= 0 = False
             | a == sumaDeDivisoresPropios a = True
             | otherwise = False

-- Cree un caso base en 0, porque sumaDeDivisoresPropios 0 = 0, y me daria True, cuando en realidad no lo es. Despues puse como condicion, si la suma de sus divisores = numero dado, entonces el numero es perfecto. De otra manera (otherwise) es False.

--ej2----------------------------------------------
--a)---------------------



listaAlicuotaDeNDeLargo:: Int -> Int -> [Int]
listaAlicuotaDeNDeLargo 0 b = []
listaAlicuotaDeNDeLargo a b =  b :( listaAlicuotaDeNDeLargo (a-1) (sumaDeDivisoresPropios b))

-- Lo que hice fue definir primero el caso base, en donde dije que a y b no pueden ser negativos (a porque es una longitud , y b porque los numeros perfectos no son negativos) y "a == 0 = []" asi el programa sabia cuando habia llegado a la longitud pedida.
-- despues le pedi "b : listaAlicuotaDeNDeLargo (a-1) (sumaDeDivisoresPropios b)" para que empezara en b la lista , es decir si le doy a = 6 y b = 10, la lista empiece [10,...]. Y en la otra parte busco que haga recurrencia:
--en "(a-1)" asi luego de "a" veces recurrencia, el programa se detiene
--y en "sumaDeDivisoresPropios b" asi agarra el siguiente b querido, por ejemplo si tenemos b = 10, => sumaDeDivisoresPropios 10 = 8, y hacerlo "a" veces.

-----------------------------------------------------------------------------------------------------------------------------------
--b)

--auxiliares--
condicion :: [Int] -> Bool
condicion a | hayrepetidos a == True = False
            | longitud a <= 2 && sumaDeDivisoresPropios (head a) == last (a) = True
            | longitud a >= 3 && sumaDeDivisoresPropios (head a) == head(tail (a)) = condicion ( tail (a))
            | otherwise = False
              
--esto nos devuelve si cumple la primera condicion o no de un club. Es decir, si la suma de los divisores del k-esimo entero coincide con el k+1-esimo. Cuando llega a longitud == 2 deja de calcular, y ahi da paso al 2do auxiliar

ultimolista:: [Int] -> Bool
ultimolista a | a == [] = False
              | hayrepetidos a == True = False
              | longitud a == 1 && esPerfecto (head (a)) == True = True
              | longitud a >= 2 && sumaDeDivisoresPropios (last (a)) == head (a) = True
              | otherwise = False 

--este auxiliar nos devuelve la 2da condicion del club. Si la suma de divisores del ultimo elemento de una lista es igual al primer numero de la lista (longitud a == 2 && sumaDeDivisoresPropios (last (a)) == head (a) = True) y el otro codigo, que abarca cuando a tiene solo un elemento (longitud a == 1 && esPerfecto (head (a)) == True = True). Ahi sabemos que para que cumpla tiene que ser un numero perfecto, ya que la suma de sus divisores es igual al mismo numero.

longitud:: [Int] -> Int
longitud n | n == [] = 0
           | otherwise = 1 + longitud (tail n)
--esto es de la clase6, calcula la longitud de una lista

hayrepe:: Int -> [Int] -> Bool
hayrepe a xs | xs == [] = False
             | a == head (xs) = True
             | otherwise = hayrepe a (tail xs)
--este es un auxiliar que utilizo para calcular otro auxiliar (hayrepetidos).                
                 
hayrepetidos:: [Int] -> Bool
hayrepetidos xs | xs == [] = False
                | hayrepe (head xs) (tail xs) == True = True
                | otherwise = hayrepetidos (tail xs)


--como me estaba quedando muy grande arriba, explico el hayrepetidos aca, por mas que lo haya usado en las funciones anteriores. Uso este programita debido a que me devuelve si hay repetidos o no. Entonces si los hubiese, ya puedo decir que esa lista no pertenece a un club.
--Por ej: [220,284] pertenece, pero [220,284,220] no pertenece (aunque cumple todas las otras condiciones) a un club.
-------------------------------------------------------------------------------------


sonSociables :: [Int] -> Bool
sonSociables [] = False
sonSociables a = ultimolista a && condicion a

-- Entonces solamente definimos que si a es vacio esto es False, y sino, tiene que cumplir que los 2 auxiliares anteriores sean True.

--ej3------------------------------------------------
--a)--
------auxiliares--------------------------------------

minimo :: [Int] -> Int
minimo (x:y:xs) | xs == [] && x > y = y
                | xs == [] && x < y = x
                | x < y = minimo (x:xs)
                | x > y = minimo (y:xs)

-- Si xs es vacio, busco minimo entre x e y. Sino busco minimo entre x e y y hago recurrencia en tail de y 

eliminarrepealfinal:: [Int] -> [Int]
eliminarrepealfinal a | a == [] = []
                      | pertenece (head a) (tail a) = (head a) : (eliminarrepealfinal (quitartodas (head a) a))
                      | otherwise = (head a) : eliminarrepealfinal (tail a)



quitar :: Int -> [Int] -> [Int]
quitar a b | b == [] = []
           | head b == a = tail b
           | head b /= a = (head b) : quitar a (tail b)

--quita un numero de una lista, si el head de b es = a , solo tiene q devolver la tail del b. sino hacer recurrencia hasta encontrar

quitartodas:: Int -> [Int] -> [Int]
quitartodas a b | b == [] = []
                | head b == a = quitartodas a (tail b)
                | head b /= a = (head b) : (quitartodas a (tail b))

--quitar frenaba cuando encontraba un numero solo = a, quitar todas saca todos los a de la lista. 
                
pertenece :: Int -> [Int] -> Bool
pertenece n m | m == [] = False
              | n == head m = True
              | otherwise = pertenece n (tail m)

--Todos estos auxiliares me sirven para calcular repetidos y el minimo de una lista, son funciones ya vistas en clases anteriores.
----------------

minimok :: Int -> Int -> [Int]
minimok k c | k == 0 = []
            | c == 0 = []
            | k == 1 && esPerfecto (c)== True = c: (minimok k (c-1))
            | k == 1 && esPerfecto (c) == False = minimok k (c-1)
            | k > 1 && sonSociables (listaAlicuotaDeNDeLargo k c) == True  = minimo (listaAlicuotaDeNDeLargo k c) : (minimok k (c-1))
            | k > 1 && sonSociables (listaAlicuotaDeNDeLargo k c) == False = minimok k (c-1)

--Esta funcion es el calculo de los minimosDeKClubesMenoresQue. Lo que trato de hacer es:
-- Para el caso de k == 1 (es decir longitud 1), solo con que el numero sea perfecto ya esta terminado. ya que es la unica posibilidad.
--en k > 1, si la lista de sus divisores (listaAlicuotaDeNDeLargo) es sociable, entonces pongo el minimo valor de ese club en la nueva lista. Sino, hago recurrencia en "c" hasta llegar al 0
--------------------

minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue k c = reverse (eliminarrepealfinal (minimok k c))

--Al final utilizo el minimok , que nos da la lista con repetidos y de mayor a menor, y le digo que deje de repetir valores (eliminarrepealfinal) y que invierta su orden (reverse). Ej: minimok 2 2000 = [1184,1184,220,220] minimosDeKClubesMenoresQue 2 2000 = [220 , 1184]

-------------------------------------
-------b)
------------------------------------------------------------------------
listaclub:: Int -> Int -> [[Int]]
listaclub n k | k == 1 = []
              | n == 0 = []
              | n == 1 && esPerfecto (k) == True = [k]: listaclub n (k-1)
              | n == 1 && esPerfecto (k) == False = listaclub n (k-1)
              | sonSociables (listaAlicuotaDeNDeLargo n k) == True && maximo (listaAlicuotaDeNDeLargo n k) <= k = (listaAlicuotaDeNDeLargo n k): listaclub n (k-1)
              | sonSociables (listaAlicuotaDeNDeLargo n k) == False || maximo (listaAlicuotaDeNDeLargo n k) >= k = listaclub n (k-1)

-- Esta funcion es la que voy a usar despues para defininir listaDeNClubesConNrosMenoresQue. Aca digo, si n == 1 , entonces [k] tiene que ser un numero perfecto. Si n > 1 , el ejercicio nos pide de encontrar los clubes tal que k > al maximo del club (que resolvemos en la linea del codigo : "maximo (listaAlicuotaDeNDeLargo n k) <= k" ). Para encontrar los clubes, buscamos que la lista de divisores de k (listaAlicuotaDeNDeLargo) que lleguemos usando la recurrencia sean sociables (sonSociables). Si estas 2 condiciones cumplen entonces guardamos su lista, sino volvemos a la recurrencia.



maximo:: [Int] -> Int
maximo (x:y:xs) | xs == [] && x > y = x
                | xs == [] && x < y = y
                | x < y = maximo (y:xs)
                | x > y = maximo (x:xs)

--Usamos este auxiliar, dado que nos pide que el maximo valor de la lista alicuota de un k, sea menor al k0 (k inicial).

------------------------AUXILIARES-------------------------------------




listaDeNClubesConNrosMenoresQue:: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue n k | n == 0 = []
                                    | k == 1 = []
                                    | otherwise = reverse(listaclub n k)


-- En esta funcion, lo que hacemos es acomodar la "listaclub", dando vuelta sus valores para que este en orden creciente.

--Ej 4 -----

amigable :: Int -> Int -> Bool
amigable a b | a == 0 = False
              | b == 0 = False
              | sumaDeDivisoresPropios a == b && sumaDeDivisoresPropios b == a = True
              | otherwise = False

esAmistosoEn :: Int -> [Int] -> Bool
esAmistosoEn n l | n == 0 = False
                 | l == [] = False
                 | amigable n (head (l)) == True = True
                 | otherwise = esAmistosoEn n (tail l)


--amigable me sirve para saber si 2 numeros son amistosos, es decir si la suma de divisores de los dos es igual al otro numero.
--Uso amigable para simplificar esAmistosoEn, en donde calculo si existe, dentro de la lista l, algun numero amistoso ocn n. Si no, hago recursion hasta que mi lista llegue a [], donde ocncluyo que es False.












