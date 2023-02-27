-- Con condicionales
fact1::Integer -> Integer
fact1 n = if n == 0 then 1 else n * fact1 (n-1)

-- Con guardas
fact2::Integer -> Integer
fact2 n
    | n == 0 = 1
    | otherwise = n * fact2 (n-1)

-- Con patrones
fact4:: Integer -> Integer
fact4 n
|n==0 = 1
|n>=1 = n * fact4 (n-1)