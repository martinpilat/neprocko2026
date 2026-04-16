-- prime n je True pokud n je prvocislo, jinak False
prime :: Integer -> Bool
prime n = all (\x -> n `mod` x /= 0) cisla
    where 
        cisla = takeWhile (\x -> x * x < n) [2..]

-- test delitelnosti cisla n cisly v seznamu cisla - vraci True pokud n neni delitelne zadnym z cisel v seznamu, jinak False
test :: Integer -> [Integer] -> Bool
test n cisla = all (\x -> n `mod` x /= 0) cisla

-- seznam prvocisel - Eratosthenovo sito
primes :: [Integer]
primes = 2:filter (\x -> test x (takeWhile (\y -> y * y <= x) primes)) [3..]

-- definice datoveho typu Barva, ktery reprezentuje barvu jako Cervena, Zelena, Modra nebo RGB s hodnotami pro cervenou, zelenou a modrou slozku
data Barva = Cervena | Zelena | Modra | RGB Int Int Int deriving Show

-- ukazka pouziti datoveho typu Barva - pattern matching pro prevod mezi Barva a RGB trojicemi
barvaNaRGB :: Barva -> (Int, Int, Int)
barvaNaRGB Cervena = (255, 0, 0)
barvaNaRGB Zelena  = (0, 255, 0)
barvaNaRGB Modra   = (0, 0, 255)

-- prevod z RGB na Barva - pokud RGB odpovida Cervena, Zelena nebo Modra, vratime odpovidajici Barva, jinak vratime RGB s hodnotami pro cervenou, zelenou a modrou slozku
rgbNaBarvu :: (Int, Int, Int) -> Barva
rgbNaBarvu (255, 0, 0) = Cervena
rgbNaBarvu (0, 255, 0) = Zelena
rgbNaBarvu (0, 0, 255) = Modra
rgbNaBarvu (x, y, z) = RGB x y z

-- definice datoveho typu Seznam, ktery reprezentuje seznam prvku typu a jako konstruktor Cons s prvkem a a zbytkem seznamu nebo konstruktor Nil pro prazdny seznam
data Seznam a = Cons a (Seznam a) | Nil deriving Show

-- pravy fold pro Seznam
foldSeznam :: (a -> b -> b) -> b -> Seznam a -> b
foldSeznam f init Nil        = init
foldSeznam f init (Cons a as) = f a (foldSeznam f init as)

-- kratsi implementace praveho foldu pro Seznam
foldSeznam2 :: (a -> b -> b) -> b -> Seznam a -> b
foldSeznam2 f init = g
    where
        g Nil = init
        g (Cons a as) = f a (g as)













