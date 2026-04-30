-- datove typy pro binarni strom a seznam z minulych cviceni
data Strom a = Uzel (Strom a) a (Strom a) | Nil deriving Show
data Seznam a = Cons a (Seznam a) | SNil deriving Show

-- pridani prvku do binarniho vyhledavaciho stromu
pridejBST :: Ord a => a -> Strom a -> Strom a
pridejBST x Nil = Uzel Nil x Nil
pridejBST x (Uzel lp y pp)
    | x < y     = Uzel (pridejBST x lp) y pp
    | otherwise = Uzel lp y (pridejBST x pp)

-- alternativni implementace pomoci foldr
seznamNaBST2::Ord a => [a] -> Strom a
seznamNaBST2 = foldr pridejBST Nil

-- map pro seznam z minuleho cviceni
seznamMap f SNil = SNil
seznamMap f (Cons a as) = Cons ma mas
    where 
        mas = seznamMap f as
        ma = f a

-- map pro strom - prochazi strom a aplikuje funkci f na kazdy prvek ve stromu
stromMap f Nil            = Nil
stromMap f (Uzel lp h pp) = Uzel mlp mh mpp
    where
        mlp = stromMap f lp
        mpp = stromMap f pp
        mh = f h

-- fold pro strom - prochazi strom a kombinuje hodnoty v uzlech pomocí funkce fUzel, hodnota pro prazdny strom je fNil
stromFold fUzel fNil Nil = fNil
stromFold fUzel fNil (Uzel lp h pp) = fUzel flp h fpp
    where
        flp = stromFold fUzel fNil lp
        fpp = stromFold fUzel fNil pp

-- sum s = foldr (+) 0 s

-- pouziti stromFold pro implementaci stromSum (soucet hodnot ve stromu) a stromInorder (vraci seznam hodnot ve stromu v inorder poradi)
stromSum strom = stromFold (\hlp h hpp -> hlp + h + hpp) 0 strom
stromInorder strom = stromFold (\hlp h hpp -> hlp ++ (h:hpp)) [] strom

-- definice datoveho typu Mozna, ktery reprezentuje hodnotu typu a, ktera muze byt Proste a nebo Nic (obdoba typu Maybe)
data Mozna a = Proste a | Nic deriving Show

-- ukazka pouziti datoveho typu Mozna - funkce safeLog a safeSqrt, ktere vraceji Mozna hodnotu pro logaritmus a odmocninu
safeLog::(Ord a, Floating a) => a -> Mozna a
safeLog x | x > 0     = Proste $ log x
          | otherwise = Nic

safeSqrt::(Ord a, Floating a) => a -> Mozna a
safeSqrt x | x >= 0     = Proste $ sqrt x
           | otherwise = Nic

-- funkce, ktera umoznuje sekvencni pouziti funkci, ktere vraceji Mozna hodnotu 
--      - pokud je vstup pro funkci f neplatny, vrati Nic, jinak aplikuje f na vstup a vrati Proste vysledek
potom Nic f = Nic
potom (Proste x) f = f x

-- alternativni implementace pomoci vestaveneho typu Maybe a
msafeLog::(Ord a, Floating a) => a -> Maybe a
msafeLog x | x > 0     = Just $ log x
          | otherwise = Nothing

msafeSqrt::(Ord a, Floating a) => a -> Maybe a
msafeSqrt x | x >= 0     = Just $ sqrt x
           | otherwise = Nothing

-- ukazka do-notace - funkce, ktera pocita sqrt(log(x) + log(x - 5))
msafeSqrtLog x = do
    y <- msafeLog x
    z <- msafeLog (x - 5)
    msafeSqrt (y + z)

-- ukazka do-notace pro seznamy - funkce, ktera vraci seznam vsech dvojic prvku z dvou seznamu (bude vysvetleno pozdeji)
uspDvojice xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- ukazka definovani toho, ze Mozna patri do trid Functor, Applicative a Monad
instance Functor Mozna where
    fmap _ Nic = Nic
    fmap f (Proste x) = Proste (f x) 

instance Applicative Mozna where
    pure x = Proste x
    
    liftA2 f Nic _ = Nic
    liftA2 f _ Nic = Nic
    liftA2 f (Proste x) (Proste y) = Proste (f x y)

instance Monad Mozna where
    (>>=) = potom

-- ukazka do-notace pro Mozna - funkce, ktera pocita sqrt(log(x) + log(x - 5))
safeSqrtLog x = do
    y <- safeLog x
    z <- safeLog (x - 5)
    safeSqrt (y + z)




