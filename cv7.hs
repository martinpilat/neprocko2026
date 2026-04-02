-- fact x = spocita faktorial x
fact :: Integer -> Integer
fact n | n < 0     = error "Chybny vstup"
       | n == 0    = 1  
       | otherwise = n * fact (n-1)

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = f x:myMap f xs 

-- mySum list = soucet prvku seznamu list 
mySum :: [Integer] -> Integer
mySum []     = 0
mySum (x:xs) = x + mySum xs

-- myFilter p list = seznam prvku z list, pro ktere plati predikat p 
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []                 = []
myFilter p (x:xs) | p x       = x:myFilter p xs
                  | otherwise = myFilter p xs

qsort [] = []
qsort (p:xs) = qsort mensi ++ p:(qsort vetsi)
    where
        mensi = filter (<=p) xs
        vetsi = filter (> p) xs
