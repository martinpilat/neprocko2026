-- data Strom a = Uzel (Strom a) a (Strom a) | Nil

import Control.Monad
import Data.Char (toUpper)

-- typ pro zasobnik, ktery bude obsahovat pouze Int
type Zasobnik = [Int]

-- push na vrchol zasobniku, vraci novy zasobnik
push::Int -> Zasobnik -> Zasobnik
push a xs = a:xs

-- pop z vrcholu zasobniku, vraci dvojici (hodnota, novy zasobnik)
pop::Zasobnik -> (Int, Zasobnik)
pop (x:xs) = (x, xs)

-- sectiDveCislaNaVrcholu z = secte dve cisla na vrcholu zasobniku, 
-- vysledek vrati a take da na vrchol zasobniku
sectiDveCislaNaVrcholu::Zasobnik -> (Int, Zasobnik)
sectiDveCislaNaVrcholu zas = 
    let 
        (a, z1) = pop zas
        (b, z2) = pop z1
        soucet  = a + b
    in
        (soucet, push soucet z2)

-- funkce nahore funguje pekne, ale musime si slozite predavat stav - libilo by se nam neco jako:
-- secti2 = 
--     a <- pop
--     b <- pop
--     let soucet = a + b
--     push soucet
--     return soucet

-- Stav je typ pro vypocty, ktere pracuji s nejakym stavem typu s a vraceji hodnotu typu a. je to tedy
-- vlastne funkce, ktera vezme stav a a vrati dvojici (hodnota, novy stav).
newtype Stav s a = Stav {runS :: s -> (a, s)}

-- typ pro push, kde je lepe videt, ze se da napsat jako Stav
--pushS::Int -> (Zasobnik -> ((), Zasobnik))
pushS::Int -> Stav Zasobnik ()
pushS a = Stav $ \ss -> ((), a:ss)

-- typ pro pop, kde je lepe videt, ze se da napsat jako Stav
--popS::(Zasobnik -> (Int, Zasobnik))
popS::Stav Zasobnik Int
popS = Stav $ \(s:ss) -> (s, ss)

-- bind operator pro Stav - dostava dva vypocty, ktere pracuji se stavem
-- a spojuje je dohromady tak, ze nejprve spusti prvni vypocet, ziska jeho vysledek a novy stav, 
-- a pak spusti druhy vypocet s timto novym stavem
bind::Stav s a -> (a -> Stav s b) -> Stav s b
(Stav sa) `bind` f = Stav $ \s -> 
    let 
        (a, s1) = sa s
        Stav sb = f a
    in sb s1 

-- ted uz mame vse potrebne pro definici Stav jako monady
instance Monad (Stav s) where
    (>>=) = bind

-- pokud definujeme Monad jako prvni, muzeme definovat Applicative a Functor pomoci funkci z Control.Monad
instance Applicative (Stav s) where
    (<*>) = ap -- ap je implemenace <*> pomoci bind operatoru
    -- pure je funkce, ktera by mela hodnotu zabalit do monady - nemeni stav, jen vraci tu hodnotu
    pure a = Stav $ \s -> (a,s) -- nebo jako Stav $ (a,)

instance Functor (Stav s) where
    fmap = liftM -- liftM je implemenace fmap pomoci bind operatoru

-- ted uz muzeme napsat funkci secti2 jako Stav pomoci "vysnene" syntaxe 
secti2::Stav Zasobnik Int
secti2 = do 
    a <- popS
    b <- popS
    let soucet = a + b
    pushS soucet
    return soucet

-- pokud chceme spustit vypocet, ktery je typu Stav s a, pouzijeme run (nebo runS definovany v record syntaxi primo u Stav), ktera vraci funkci ulozenou ve vypoctu  
run::Stav s a -> (s -> (a, s))
run (Stav sa) = sa

-- ukazka implementace funkci ap a liftM pomoci bind operatoru
myAp :: Monad m => m (a -> b) -> m a -> m b
mab `myAp` ma = 
    do 
        ab <- mab
        a <- ma
        return $ ab a

myLiftM::Monad m => (a -> r) -> m a -> m r
myLiftM f ma = 
    do
        a <- ma
        return $ f a

-- predavi string na velka pismena
zakric = map toUpper

-- ukazka prace s IO monadou - nacte obsah souboru, prevede ho na velka pismena a vypise na obrazovku
main::IO ()
main = do
    text <- readFile "cv3.pl"
    let velky = zakric text
    putStrLn velky

-- pro praci se standardnim vstupem lze taky pouzit funkci interact, ktera nacte vstup, 
-- preda ho funkci a vypise vysledek do standardniho vystupu
main2::IO ()
main2 = interact zakric



