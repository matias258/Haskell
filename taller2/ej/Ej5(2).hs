sumaDivdesde :: Int->Int->Int
sumaDivdesde n k | k == n = n
                 | mod n k == 0 = k + sumaDivdesde n (k+1)
                 | otherwise = sumaDivdesde n (k+1)











