-- flatten xs vrati seznam, ktery vznikne spojenim vsech seznamu v xs 
flatten::[[a]] -> [a]
flatten = foldl (++) []

-- dezip rozebere seznam dvojic na dvojici seznamu
dezip [] = ([], [])
dezip ((a, b):s) = (a:as, b:bs) 
    where
        (as, bs) = dezip s

-- totez, ale pomoci let misto where
dezip2 []  = ([], [])
dezip2 ((a, b):s) = let (as, bs) = dezip2 s
    in
        (a:as, b:bs) 

-- totez bez let a where - muze byt pomalejsi, protoze se dezip_slow vola dvakrat pro kazdy prvek seznamu
dezip_slow [] = ([], [])
dezip_slow ((a, b):s) = (a:(fst (dezip_slow s)), 
                         b:(snd (dezip_slow s)))

-- rev xs vrati seznam, ktery vznikne obracenim poradi prvku v xs
rev xs = rev' xs []
rev' [] acc = acc
rev' (x:xs) acc = rev' xs (x:acc)

-- typ pro dvojici prvku stejneho typu
type Pair a = (a,a)

-- prvni p vrati prvni prvek dvojice p
prvni:: Pair a -> a
prvni = fst

-- definice typu pro binarni strom
data Strom a = Uzel (Strom a) a (Strom a) | Nil deriving Show

-- pridani prvku do binarniho vyhledavaciho stromu
pridejBST :: Ord a => a -> Strom a -> Strom a
pridejBST x Nil = Uzel Nil x Nil
pridejBST x (Uzel lp y pp)
    | x < y     = Uzel (pridejBST x lp) y pp
    | otherwise = Uzel lp y (pridejBST x pp)

-- alternativni zapis pomoci if-then-else
pridej x Nil = Uzel Nil x Nil
pridej x (Uzel lp y pp) = if x < y then Uzel (pridej x lp) y pp
                            else Uzel lp y (pridej x pp) 

-- funkce, ktera prohodi poradi argumentu dane funkce
myFlip f a b = f b a

-- prevede seznam na binarni vyhledavaci strom 
seznamNaBST::Ord a => [a] -> Strom a
seznamNaBST = foldl (flip pridejBST) Nil 

-- alternativni implementace pomoci foldr
seznamNaBST2::Ord a => [a] -> Strom a
seznamNaBST2 = foldr pridejBST Nil

