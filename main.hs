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
  where (q, as) = bestQuestion [ x | x <- dat, not $ elem False (map (flip elem (snd x)) mask) ] mask

printa :: String -> (Char, Tree) -> IO ()
printa s n = do
  putStr   s
  putStrLn ((fst n):[])
  printt (' ':s) (snd n) 

printt :: String -> Tree -> IO ()
printt s (Question (q , ts)) = do
  putStr   s
  putStrLn q
  mapM_ (printa (' ':s)) ts 

printt s (Decision d) = do
  putStr   s
  putStrLn $ show d


isAnswer :: String -> (Char, Tree) -> Bool
isAnswer (a:_) (g, _) = a == g

guess :: Tree -> [String] -> IO ()
guess (Question t) a:as = do
  putStrLn (fst t)
  let an = find (isAnswer a) (snd t)
  case an of 
    Nothing -> putStrLn "??" 
    Just n   -> guess (snd n) as

guess (Decision d) = do
  putStr "This mushrom is " 
  putStrLn $ show d

main :: IO ()
main = do
    putStrLn "Readig data file: agaricus-lepiota.dat"
    contents <- readFile "agaricus-lepiota.data"
    let entries = splitData contents
    let dat = prepData entries
    seq dat (putStrLn "Parsing file for computation.")
    let tree= makeTree dat []
    seq tree (putStrLn "Computing tree.")
    putStrLn "Starting guess."
    a <- getContents 
    let answers = lines a
    guess tree answers

    --putStrLn "Tree strocture"
    --printt "" tree