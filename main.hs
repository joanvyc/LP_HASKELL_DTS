--import Data.List.Split

data Desicion = Edible | Poisnous

data Tree = Question (String , [(Char, Tree)]) | Desicion

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

answer_name_tag :: [[(String, Char)]]
answer_name_tag = map (\a <- zip (fst a) (fst a)) $ zip answer_name answer_tags
    
parseLine :: [Char] -> (Desicion, [(String, Char)])
parseLine (x:xs) 
  | x == 'e'  = (Edible,   zip question_names xs)
  | otherwise = (Poisnous, zip question_names xs)

prepData :: [[Char]] -> [(Desicion, [(String, Char)])]
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

qa_names :: [(String, [String])]
qa_names = zip question_names answer_name    

bestQuestion :: [(Desicion, [(String, Char)])] -> (String, [(Char, Desicion, Real)])
bestQuestion dat = map (\(p, as) -> (q, map (\a -> (q, , ) where fdat = [x | x <- dat, elem (q,a) (snd x)]) as) qa_names

makeTree :: [(Desicion, [(String, Char)])] -> [(String, Char)] -> Tree
makeTree dat mask = Question (q , map (\ x -> case x of
                                         (_,d,0) -> d
                                         (a,_,p) -> (a, makeTree dat (q,a):mask)
                                         ) as )
  where (q, as) = bestQuestion [ x | x <- dat, not $ elem False (map (flip elem (snd x)) mask) ]

main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    let entries = splitData contents
    let dat = prepData entries
    makeTree dat emptyTree []