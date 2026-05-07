import Text.Read -- for readMaybe
import Control.Monad -- for guard

-- ukazka typu Either a case konstrukce
nacti::String -> Either String Int
nacti str = 
    case (readMaybe str :: Maybe Int) of
        Just x -> Right x
        Nothing -> Left str

-- vyhledani v seznamu, vraci vsechny odpovidajici hodnoty
najdi::(Eq a) => a -> [(a,b)] -> [b]
najdi _ [] = []
najdi a ((x, y):xs) 
    | a == x = y:najdi a xs
    | otherwise = najdi a xs

-- to same pomoci list comprehensions
najdi2 a xs = [y | (x, y) <- xs, x == a]

-- to same pomoci filter a map
najdi3 a xs = map snd $ filter (\x -> a == fst x) xs

-- to same pomoci do notace (ukazka prepisu list comprehension do do notace)
najdi4 a xs = do
    (x, y) <- xs
    guard $ x == a
    return y

-- seznamy pro testovani
l1 = [(1, 'a'), (2, 'b'), (1, 'c')]
l2 = [('a', 221), ('c', 3342), ('b', 333), ('a', 456)]

-- napred najde v seznmu xs vsechny odpovidajici hodnoty, a pak pro kazdou z nich najde v seznamu ys vsechny odpovidajici hodnoty
-- doubleNajdi::(Eq a, Eq b) => a -> [(a, b)] -> [(b, c)] -> [c]
doubleNajdi a xs ys = 
    let 
        nalezene = najdi a xs
    in 
        concatMap (\x -> najdi x ys) nalezene


-- to same pomoci do notace
doubleNajdi2 a xs ys = do 
    x <- najdi a xs
    najdi x ys

-- to same pomoci bind operatoru (ukazka prepisu do notace do bind operatoru)
doubleNajdi3 a xs ys = najdi a xs >>= (\x -> najdi x ys)

-- pro kazdy prvek z xs a pro kazdy prvek z ys vytvori dvojici (x, y)
uspDv xs ys = [(x, y) | x <- xs, y <- ys]

-- to same pomoci do notace
uspDv2 xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- to same pomoci bind operatoru (ukazka prepisu do notace do bind operatoru)
uspDv3 xs ys = xs >>= (\x -> ys >>= (\y -> return (x, y)))

-- ukazka typove tridy Monoid, ktera ma asociativni operaci (<>, mappend) a neutralni prvek (mempty)
-- trida pro Monoid s nasobenim a jednotkou jako neutralni prvek
newtype Mul a = M a deriving Show

-- monoid musi byt i pologrupa
instance Num a => Semigroup (Mul a) where
    (M x) <> (M y) = M (x * y)

instance Num a => Monoid (Mul a) where
    mempty = M 1

-- trida pro Monoid se scitanim a nulou jako neutralni prvek
newtype Sum a = S a deriving Show

instance Num a => Semigroup (Sum a) where
    (S x) <> (S y) = S (x + y)

instance Num a => Monoid (Sum a) where
    mempty = S 0

-- mconcat [S 1, S 2, S 3, S 4] = S 10
-- mconcat [M 1, M 2, M 3, M 4] = M 24
