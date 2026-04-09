-- seznamDvakrat x je seznam kde je kazdy prvek x vynasobeny 2
seznamDvakrat = map (*2)

-- uncurry f dela z funkce, ktera bere dve hodnoty funkci, ktera bere jednu dvojici
myUncurry f (a, b) = f a b

-- myzip xs ys je seznam dvojic (xs[i], ys[i]) pro seznamy xs, ys 
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys

-- mySum list = soucet prvku seznamu list 
mySum :: [Integer] -> Integer
mySum []     = 0
mySum (x:xs) = (+) x (mySum xs)

-- myMax list = maximum prvku seznamu list 
myMax :: [Integer] -> Integer
myMax []     = 0
myMax (x:xs) = max x (myMax xs)

-- myDivR list = podil prvku seznamu list (uzavorkovany doprava)
myDivR :: Fractional a => [a] -> a
myDivR []     = 1.0
myDivR (x:xs) = (/) x (myDivR xs)

-- myDivL list = podil prvku seznamu list (uzavorkovany doleva)
myDivL a [] = a
myDivL a (x:xs) = myDivL (a/x) xs

-- myFoldr f a xs je pravy fold seznamu xs (zobecneni mySum, myMax, myDivR)
myFoldr f a []     = a
myFoldr f a (x:xs) = f x (myFoldr f a xs)

mySum2  = myFoldr (+) 0
myMax2  = myFoldr max 0
myDivR2 = myFoldr (/) 1.0

-- myFoldl f a xs je levy fold seznamu xs
myFoldl f a []     = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs

-- myAnd xs je and vsech prvku seznamu xs
myAnd::[Bool] -> Bool
myAnd = foldr (&&) True

-- myAnd2 xs je to same jako myAnd ale pomoci foldl
myAnd2::[Bool] -> Bool
myAnd2 = foldl (&&) True

-- foldrMap f xs je map f xs napsany pomoci foldr
foldrMap::(a -> b) -> [a] -> [b]
foldrMap f = foldr g []
    where
        g x = (:) (f x)

