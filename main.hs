
import Data.List

data MClass = Edible | Poisnous | Indetermined deriving (Eq, Show)

data Tree = Question (String , [(Char, Tree)]) | Decision MClass deriving (Show)

question_names :: [String]
question_names = [
    "cap-shape",
    "cap-surface",
    "cap-color",
    "bruises?",
    "odor",
    "gill-attachment",
    "gill-spacing",
    "gill-size",
    "gill-color",
    "stalk-shape",
    "stalk-root",
    "stalk-surface-above-ring",
    "stalk-surface-below-ring",
    "stalk-color-above-ring",
    "stalk-color-below-ring",
    "veil-type",
    "veil-color",
    "ring-number",
    "ring-type",
    "spore-print-color",
    "population",
    "habitat"]
    
answer_tags :: [[Char]]
answer_tags = [
  ['b','c','x','f','k','s'],
  ['f','g','y','s'],
  ['n','b','c','g','r','p','u','e','w','y'],
  ['t','f'],
  ['a','l','c','y','f','m','n','p','s'],
  ['a','d','f','n'],
  ['c','w','d'],
  ['b','n'],
  ['k','n','b','h','g','r','o','p','u','e','w','y'],
  ['e','t'],
  ['b','c','u','e','z','r','?'],
  ['f','y','k','s'],
  ['f','y','k','s'],
  ['n','b','c','g','o','p','e','w','y'],
  ['n','b','c','g','o','p','e','w','y'],
  ['p','u'],
  ['n','o','w','y'],
  ['n','o','t'],
  ['c','e','f','l','n','p','s','z'],
  ['k','n','b','h','r','o','u','w','y'],
  ['a','c','n','s','v','y'],
  ['g','l','m','p','u','w','d']]
  
answer_name :: [[String]]
answer_name = [
    ["bell","conical","convex","flat","knobbed","sunken"],
    ["fibrous","grooves","scaly","smooth"],
    ["brown","buff","cinnamon","gray","green","pink","purple","red","white","yellow"],
    ["bruises","no"],
    ["almond","anise","creosote","fishy","foul","musty","none","pungent","spicy"],
    ["attached","descending","free","notched"],
    ["close","crowded","distant"],
    ["broad","narrow"],
    ["black","brown","buff","chocolate","gray","green","orange","pink","purple","red","white","yellow"],
    ["enlarging","tapering"],
    ["bulbous","club","cup","equal","rhizomorphs","rooted","missing"],
    ["fibrous","scaly","silky","smooth"],
    ["fibrous","scaly","silky","smooth"],
    ["brown","buff","cinnamon","gray","orange","pink","red","white","yellow"],
    ["brown","buff","cinnamon","gray","orange","pink","red","white","yellow"],
    ["partial","universal"],
    ["brown","orange","white","yellow"],
    ["none","one","two"],
    ["cobwebby","evanescent","flaring","large","none","pendant","sheathing","zone"],
    ["black","brown","buff","chocolate","green","orange","purple","white","yellow"],
    ["abundant","clustered","numerous","scattered","several","solitary"],
    ["grasses","leaves","meadows","paths","urban","waste","woods"]]

get3rd :: (a,b,c) -> c
get3rd (_,_,x) = x

get2nd :: (a,b,c) -> b
get2nd (_,x,_) = x

get1st :: (a,b,c) -> a
get1st (x,_,_) = x

--answer_name_taganswer_name_tag :: [[(String, Char)]]
--answer_name_tag = map (\a -> map (\b -> (fst a, b)) (snd a)) (zip answer_name answer_tags)
    
parseLine :: [Char] -> (MClass, [(String, Char)])
parseLine (x:xs) 
  | x == 'e'  = (Edible,   zip question_names xs)
  | otherwise = (Poisnous, zip question_names xs)

prepData :: [[Char]] -> [(MClass, [(String, Char)])]
prepData dat = map parseLine dat 

-- Funcio: splitLine
-- -----------------
-- Per parsejar l'entrada passa d'una linia a un array de chars

splitLine :: [Char] -> [Char]
splitLine [] = []
splitLine (x:xs) 
  | x == ','  = splitLine xs
  | otherwise = x : splitLine xs

--
-- Funcio: splitDara
-- -----------------
-- Per parsejar l'entrada passa d'un unic String a una llista d'estrings separats
-- per salt de linia.

splitData :: String -> [[Char]]
splitData ss = map splitLine (lines ss)

qa_names :: [(String, [Char])]
qa_names = zip question_names answer_tags 

qa_aLookup = zip question_names $ zipWith zip answer_tags answer_name

calcFactor :: [(MClass, [(String, Char)])] -> (MClass, Float, Float)
calcFactor dat 
  | (ldat Edible) <= (ldat Poisnous) 
    = (Poisnous, (ldat Poisnous), (ldat Poisnous) + (ldat Edible))
  | otherwise
    = (Edible,   (ldat Edible  ), (ldat Poisnous) + (ldat Edible))
  where 
    ldat d = fromIntegral $ length [ x | x <- dat, fst x == d] 


--ordTup :: (MClass, [(String, Char)]) -> (MClass, [(String, Char)]) -> Eq
ordTup (_, xs) (_, ys)
  | calc xs > calc ys = GT
  | calc xs < calc ys = LT
  | otherwise = EQ
  where 
    calc [] = 1
    calc ((_,_,c):cs) = (c) + calc cs


bqmk_mapk :: [(MClass, [(String, Char)])] -> String -> Char -> (Char, MClass, Float)
bqmk_mapk dat q a = let fdat = calcFactor [x | x <- dat, elem (q,a) (snd x)] in (a, get1st fdat, ((get2nd fdat) / (fromIntegral $ length dat)))

bq_mapk :: [(MClass, [(String, Char)])] -> (String, [Char]) -> (String, [(Char, MClass, Float)])
bq_mapk dat (q, as) = (q, map (bqmk_mapk dat q) as)

bestQuestion :: [(MClass, [(String, Char)])] -> [(String, Char)] -> (String, [(Char, MClass, Float)])
bestQuestion dat mask = head (sortBy ordTup (map (bq_mapk dat) [ x | x <- qa_names, not $ elem (fst x) (fst (unzip mask)) ]))

decideNode :: [(MClass, [(String, Char)])] -> [(String, Char)] -> String -> (Char, MClass, Float) -> (Char, Tree)
decideNode _ _ _ (a,d,0) = (a, Decision d)
decideNode dat mask q (a,d,_)
  | ((length mask)+1) == (length qa_names) = (a, Decision d)
  | otherwise                              = (a, makeTree dat ((q,a):mask))

makeTree :: [(MClass, [(String, Char)])] -> [(String, Char)] -> Tree
makeTree dat mask
  | (length mask) == (length qa_names) = Decision Indetermined 
  | otherwise = Question (q , map (decideNode dat mask q) as)
  where (q, as) = bestQuestion [ x | x <- dat, and (map (flip elem (snd x)) mask) ] mask

printa :: String -> (Maybe String, Tree) -> IO ()
printa s (Just n1, n2) = do
  putStr   s
  putStrLn ('a':':':n1)
  printt (' ':s) n2 

printa s (Nothing, _) = do
  putStr   s
  putStrLn "ERROR"

printt :: String -> Tree -> IO ()
printt s (Question (q , ts)) = do
  putStr   s
  putStrLn ('?':':':q)
  let qr = case lookup q qa_aLookup of Nothing -> []; Just n -> n;
  let ar = map (\a -> (lookup (fst a) qr, snd a)) ts
  mapM_ (printa (' ':s)) ar

printt s (Decision d) = do
  putStr   s
  putStrLn $ show d

nombreEspais_ :: Int -> String -> (Int, String)
nombreEspais_ n [] = (n, [])
nombreEspais_ n s1@(s:ss)
  | s == ' '  = nombreEspais_ (n+1) ss
  | otherwise = (n, s1)

nombreEspais :: String -> (Int, String)
nombreEspais = nombreEspais_ 0

{-
construeixArbre :: Int -> [(Int, String)] -> Tree
construeixArbre n (x:xs) 
  | n == fst x = 
-}

{-
formatejarArbre :: [String] -> Tree
formatejarArbre = construeixArbre . map nombreEspais 
-}

{- chaks is the tuple (answer, Tree) corresponds with the given answer -} 
esResposta :: String -> (Char, Tree) -> Bool
esResposta [] _ = False
esResposta (a:_) (g, _) = a == g

{- if the tree is NOT of type Decision is True, otherwise False -} 
noEsDecisio :: Tree -> Bool
noEsDecisio (Decision _) = False
noEsDecisio _ = True

{- if the tree is a decision returns its string otherwise returns the question -} 
showArbre :: Tree -> String
showArbre (Question (q,_)) = q
showArbre (Decision    d) = show d

{- given a tree and an answer returs the corresponding tree -} 
seguentArbre :: Tree -> [Char] -> Tree
seguentArbre (Question t) a = case find (esResposta a) (snd t) of
  Nothing -> Decision Indetermined
  Just t  -> snd t

{- like takeWhile but also keeps the element brekes the take -}
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = foldr (\x ys -> if p x then x:ys else [x]) []

{-  Generating decision tree -}
generarArbre :: IO ()
generarArbre = do
  dadesAlFitcher <- readFile "agaricus-lepiota.data"
  let conjuntDeDades = (map parseLine . map (filter (/= ',')) . lines) dadesAlFitcher
  let arbreDecisions = makeTree conjuntDeDades []
  printt "" arbreDecisions

{- Prediction based on the decision tree -}
{-
ferPrediccio :: IO ()
ferPrediccio = do
  dataInFile <- readFile "arbre-de-decisions.dat"
  let arbreDecisions = formatejarArbre . lines dataInFile
  interact $
    unlines .                                     -- Joins all output lines
    map showArbre .                                -- Translates tree to strign
    takeWhile1 noEsDecisio .              -- Takes trees 'til decision 
    scanl seguentArbre arbreDecisions .
    lines
      -}

main :: IO ()
main = do 
  generarArbre

  
