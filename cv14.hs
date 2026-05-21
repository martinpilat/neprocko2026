-- foldl' je strict verze foldl - vyhodnocuje akumulátor okamžitě
import Data.List (foldl')

-- vypíše každý prvek seznamu na nový řádek
printListAsLines::(Show a) => [a] -> IO ()
printListAsLines [] = do return ()
printListAsLines (x:xs) = do
    putStrLn $ show x
    printListAsLines xs

-- to samé ale pomocí map a sequence
printListAsLinesS xs = do
    _ <- sequence $ map (putStrLn . show) xs -- nebo ještě jednodušeji: mapM (putStrLn . show) xs
    return () -- tohle zajistí, že nevracíme nic

-- funkce pro výpis seznamu na řádky - generátor čísel
-- main :: IO ()
-- main = do
--     printListAsLinesS [1..10000000]

-- vypočítá součet čísel zapsaných ve vstupním řetězci, kde každé číslo je na samostatném řádku
zpracuj :: String -> String
zpracuj vstup = show $ foldl (+) 0 $ map (\x -> read x::Int) $ lines vstup

-- načte soubor "cisla.txt", zpracuje jeho obsah pomocí funkce zpracuj a vypíše výsledek
main :: IO ()
main = do
    cisla <- readFile "cisla.txt"
    let vysledek = zpracuj cisla
    putStrLn vysledek

--------------------------------------------------------
-- implementace Turingova stroje

-- směr pohybu hlavy
data Dir = L | R | N

-- Turingův stroj, parametrizovaný typy pro symboly na pásce a pro stavy
data TS a b = TS { tape         :: ([a], a , [a]) -- páska reprezentovaná jako trojice: levá část, symbol pod hlavou, pravá část
                  , state       :: b              -- stav Turingova stroje
                  , function    :: (a,b) -> [(a, b, Dir)] -- přechodová funkce, která pro daný symbol a stav vrací seznam možných akcí
                  , acc_states  :: [b] -- seznam přijímajících stavů
                  , pos         :: Int -- aktuální pozice hlavy (0 je výchozí pozice) (pro výpis)
                  , minPos      :: Int -- minimální dosažená pozice hlavy (pro výpis)
                  , maxPos      :: Int -- maximální dosažená pozice hlavy (pro výpis)
                  }

-- pro výpis Turingova stroje - výstup je "stav: páska", kde páska je zobrazená jako String, symbol pod hlavou je zvýrazněn pomocí hranatých závorek
instance (Show a, Show b) => Show (TS a b) where
    show ts = show (state ts) ++ ": " ++ tapeStr
        where
            (lt, h, rt) = tape ts
            ltape = concat $ map show $ reverse $ take ((pos ts) - (minPos ts) + 1) lt
            rtape = concat $ map show $ take ((maxPos ts) - (pos ts) + 1) rt
            tapeStr = ltape ++ "[" ++ (show h) ++ "]" ++ rtape

-- jeden krok TS
step ts = do
    let (l:ltape, head, r:rtape) = tape ts
    let f = function ts
    let s = state ts
    let p = pos ts
    let minP = minPos ts
    let maxP = maxPos ts
    (nh, ns, dir) <- f (head, s) -- zde využíváme list monádu pro výběr jednoho z možných přechodů
    case dir of -- aktualizace pásky a pozice hlavy podle směru pohybu + ukázka record syntax pro aktualizaci polí v TS
        L -> return $ ts {tape = (ltape, l, nh:r:rtape), 
                          state = ns, 
                          pos = p - 1,
                          minPos = min minP (p - 1)}
        R -> return $ ts {tape = (nh:l:ltape, r, rtape), 
                          state = ns, 
                          pos = p + 1,
                          maxPos = max maxP (p + 1)}
        N -> return $ ts {tape = (l:ltape, nh, r:rtape), state = ns} 

-- příklad přechodové funkce TS, který z pásky samých 0 vytvoří 010201020102...
f (0, 'b') = [(1, 'c', R)]
f (0, 'c') = [(0, 'e', R)]
f (0, 'e') = [(2, 'f', R)]
f (0, 'f') = [(0, 'b', R)]

-- pomocná funkce pro vytvoření TS s danou počáteční páskou, stavem, příjímajícími stavy a přechodovou funkcí 
makeTS (i:initTape) initState accState f =
    TS (repeat 0, i, initTape ++ repeat 0) initState f accState 0 0 (length initTape)

-- simulace maxSteps kroků TS, výstupem je seznam všech možných konfigurací TS po maximálně maxSteps krocích
sim ts 0 = return ts
sim ts maxSteps = do  
        ns <- step ts
        if state ns `elem` acc_states ts then -- stroje v příjímajícím stavu se již dále nesimulují
            return ns 
        else
            sim ns (maxSteps - 1)
