{-
Aquest programa forma part de un treball de l'assignatura Llenguatges de
Programacio (LP) de la Facultat d'Informatica de Barcelona (FIB). I consisteix
implementar la construccio i navegacio d'un arbre de decisions, en Haskell.
-}

{-
Com que necesiterem ordenar llistes i buscar-hi elements incloem aquest modul
(Data.List) que implementa les funcions sortBy i find (per ordenar i buscar, 
respectivament)
-}
import Data.List

{-
Com que per poder utilitzar les diferents funcionalitats del programa utilitzem 
les opcions del programa (args) incloem el modul System.Environment. Tambe 
utilitzarem el modul System.Exit per poder finalizar el programa amb codis 
d'error.
-}
import System.Environment
import System.Exit


{------------------------------------------------------------------------------
\                             ABSTRACCIO DE DADES                             /
 -----------------------------------------------------------------------------}

{-
Necesiterem identificar els diferents conceptes que hi ha en l'entorn del
conjunt de dades que se'ns proporciona.  Per aixo s'ha definit el tipus de 
dades MClass i s'han creat les funcions question_names, answer_name i 
answer_tags.
-} 

{-
El tipus de dades MClass es fa servir per representar la comestibilitat d'un 
bolet, i te els estats Comestible, Verinos i Inteterminat. El cas Indeterminat
s'utilitza per indicar que hi ha hagut un error, ja sigui per determinar l'estats
o per navegar l'arbre.
-}
data MClass = Comestible | Verinos | Indeterminat deriving (Eq, Show)

{-
Per tenir les possibles preguntes que hi poden haver sobre un bolet, es 
representa amb una llista, seguint l'ordre de la documentacio (que es pot
trobar a: https://archive.ics.uci.edu/ml/datasets/Mushroom)
-}
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
    
{- 
Tambe necesitarem les possibles respostes a cada pregunta, per aixo definim una 
llista on cada element es una llista amb les possibles etiquetes de resposta.
Quan es fa referencia a etiqueta, tracta de l'abreviacio a un caracter d'una 
resposta, i es unic per cada resposta d'una pregunta.
-} 
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
  
{-
Per tenir constancia de quina es realment la resposta, que es correspont a cada 
etiqueta construim una llista de llistes amb les mateixes dimencions i el pateix 
ordre que answer_tags. Que ens associen cada etiqueta amb la resposta.
-} 
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

{- 
A mes a mes tambe ens poden ser utils combinacions d'aquestes llistes.
-}

{-
Aquesta estroctura que es simplement ajuntar les dues questions amb les respostes
sera la principal estroctura que es fara servir per construir l'arbre.
-}
qa_names :: [(String, [Char])]
qa_names = zip question_names answer_tags 

{-
Aquesta combinacio de preguntes i respostes ens servira com a taula de treduccio 
par traduir de l'etiqueta al nom senser per a les respostes d'una pregunta.
-}
qa_aLookup :: [(String, [(Char, String)])]
qa_aLookup = zip question_names $ zipWith zip answer_tags answer_name

{-
Per poder satisfer l'objectiu del programa cal construir un tipus de dades
per fer l'arbre. En aquest programa l'anomenem Tree, i es pot satisfer de dues 
maneres la primera son els nodes interns que anomenem Question's que estan formats 
per una tupla String - Lista de (tupla Char-Tree) on la cadena de 
caracters (Primer element de la tupla mes externa) es la pregunta, i la llista
associa cada possible resposta (primer element de les tuples internes) amb un 
arbre (segon element de les tuples internes). La segona manera de la que es pot 
satisfer es amb una decissio que son les fulles de l'arbre.
-}
data Tree = Question (String , [(Char, Tree)]) | Decision MClass deriving (Show)

{- 
Per fer mes amena la llectura del codi el tipus de dades que conte les 
dades del fitxer un cop parsejades, construim aquest tipus de dades que 
anomenem PData (de Parsed Data)
-}
type LPData = (MClass, [(String, Char)])
type PData  = [LPData]

{------------------------------------------------------------------------------
\                                FUNCIONS UTILS                               /
 -----------------------------------------------------------------------------}

{-
Per tractar amb tuples de tres elements s'han implementat rest funcions. 
get1sr i get2nd que son les analogues de fst i snd per tuples de dos elements, i 
get3rd que retorna el tercer element de la tupla.
-}

{- Retorna el 1r element -}
get1st :: (a,b,c) -> a
get1st (x,_,_) = x

{- Retorna el 2r element -}
get2nd :: (a,b,c) -> b
get2nd (_,x,_) = x

{- Retorna el 3r element -}
get3rd :: (a,b,c) -> c
get3rd (_,_,x) = x

{- like takeWhile but also keeps the element brekes the take -}
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = foldr (\x ys -> if p x then x:ys else [x]) []

swapTup :: (a,b) -> (b, a)
swapTup (x, y) = (y, x)

--ordTup2 :: (MClass, [(String, Char)]) -> (MClass, [(String, Char)]) -> Eq
ordTup2 (_, xs) (_, ys)
  | calc xs > calc ys = GT
  | calc xs < calc ys = LT
  | otherwise = EQ
  where 
    calc [] = 0
    calc ((_,_,c):cs) = (c) + calc cs


{------------------------------------------------------------------------------
\                                   NUCLI                                     /
 -----------------------------------------------------------------------------}

{-
Per tal de determinar quina es la pregunta que te mes probabilitats d'encert
crearem un vector que hi haura cada pregunta que queda per utilitzar, aquest
llista tindra l'identificador de la pregunta, la opcio de comestibilitat cap a
la que es decanta la pregunta i la probabilitat d'encert. Despres ordanarem 
aquesta llista i ens quedarem el primer element
-}

{-
Calcula el factor d'encert per a un conunt de dades, tenint en compte el nombre 
de entrades que s'analitzen.
-}
{- Sempre seleccionem la comestibilitat amb mes probabilitat d'encert. -}
calcFactor :: PData -> (MClass, Float, Float)
calcFactor dat 
  | (ldat Comestible) <= (ldat Verinos) 
    = (Verinos, (ldat Verinos), (ldat Verinos) + (ldat Comestible))
  | otherwise
    = (Comestible,   (ldat Comestible  ), (ldat Verinos) + (ldat Comestible))
  where 
    ldat d = fromIntegral $ length [ x | x <- dat, fst x == d] 

{- 
Per una pregunta concreta determina l'opcio preferida i en quin percentatge 
apareix al subconjunt de dades que se li ha passat.
-}
recomptePerResposta :: PData -> String -> Char -> (Char, MClass, Float)
recomptePerResposta dat q a = let fdat = calcFactor [x | x <- dat, elem (q,a) (snd x)] 
  in (a, get1st fdat, ((get2nd fdat) / (fromIntegral $ length dat)))

{- 
Fem el recompte per a totes les prefuntes que queden per evaluar i les agupa a una 
tupla String-recompte on la cadena de caracters identifica la pregunta i el recompte
conte la resposta l'opcio preferida i la probabilitat d'encert.
-}
recompte :: PData -> (String, [Char]) -> (String, [(Char, MClass, Float)])
recompte dat (q, as) = (q, map (recomptePerResposta dat q) as)

{-
Aquesta funcio determina quina es la millor pregunta per determinar la classe 
dels bolets. Per aixo crea una llista ordenada per l'euristica i en retorna 
el primer element.
-}
bestQuestion :: PData -> [(String, Char)] -> (String, [(Char, MClass, Float)])
bestQuestion dat mask = head 
  $ sortBy ordTup2 
  $ map (recompte dat) [ x | x <- qa_names, not $ elem (fst x) (fst (unzip mask)) ]

{- 
Determina si el ja s'ha de determinar una decisio o si encara es pot precissar amb 
mes preguntes, i retorna una Decision o un nou arbre en funcio d'aixo.
-}
decideNode :: PData -> [(String, Char)] -> String -> (Char, MClass, Float) -> (Char, Tree)
decideNode _ _ _ (a,d,0) = (a, Decision d)
decideNode dat mask q (a,d,_)
  | ((length mask)+1) == (length qa_names) = (a, Decision d)
  | otherwise                              = (a, makeTree dat ((q,a):mask))

{-
Donat un conjunt de dades crea un arbre de decisio a partir de la millor pregunta, i
les probabilitats d'encert de cada resposta per seguir construint l'arbre.
-}
makeTree :: PData -> [(String, Char)] -> Tree
makeTree dat mask
  | (length mask) == (length qa_names) = Decision Indeterminat -- Si el programa funciona no s'hi arriba
  | otherwise = Question (q , map (decideNode dat mask q) as)
  where (q, as) = bestQuestion [ x | x <- dat, and (map (flip elem (snd x)) mask) ] mask


{------------------------------------------------------------------------------
\                               ENTRADA / SORTIDA                             /
 -----------------------------------------------------------------------------}

{-
Tradueix les dades del fitxer amb el conjunt de dades a l'estroctura utilitzada
principalment PData.
-}
parseLine :: [Char] -> LPData
parseLine (x:xs) 
  | x == 'e'  = (Comestible, zip question_names xs)
  | otherwise = (Verinos,    zip question_names xs)

{-
Es una crida co-recursiva amb printt. Imprimeix la resposta a una pregunta i crida
a la funcio printt per seguir imprimint l'arbre
-}
printa :: String -> (Maybe String, Tree) -> IO ()
printa s (Just n1, n2) = do
  putStr   s
  putStrLn (n1)
  printt (' ':s) n2 

printa s (Nothing, _) = do
  putStr   s
  putStrLn "ERROR"

{-
Es una crida co-recursiva amb printa. Imprimeix una pregunta i crida recursiva a
printa per imprimir la part corresponent a les respostes.
-}
printt :: String -> Tree -> IO ()
printt s (Question (q , ts)) = do
  putStr   s
  putStrLn (q)
  let qr = case lookup q qa_aLookup of Nothing -> []; Just n -> n;
  let ar = map (\a -> (lookup (fst a) qr, snd a)) ts
  mapM_ (printa (' ':s)) ar

printt s (Decision d) = do
  putStr   s
  putStrLn $ show d


{- 
Comproba que el primer parametre sigui un caracter (agafa el primer element, ignora la
resta) (que correspont a l'etiqueta d'una resposta) sigui iguala a la resposta de la 
tupla (segon parametre) (etiqueta de la pregunta)
 -} 
esResposta :: Char -> (Char, Tree) -> Bool
esResposta a (g, _) = a == g

{- 
Retorna cert si i nomes si l'arbre es una Decision
-} 
noEsDecisio :: Tree -> Bool
noEsDecisio (Decision _) = False
noEsDecisio _ = True

{- 
Si un arbre es una Question retorna una pregunta en format cadena de caracters.
-} 
mostreArbre :: Tree -> String
mostreArbre  (Decision Indeterminat) = "Hi ha hagut algun problema."
mostreArbre         (Question (q,_)) = "Which " ++ q ++ "?"
mostreArbre          (Decision    d) = "Aquest bolet es " ++ (show d)


{------------------------------------------------------------------------------
\                                PRINCIPAL                                    /
 -----------------------------------------------------------------------------}

tNomAEtiq :: String -> String -> Char
tNomAEtiq q a =
  case trad of
   Nothing -> '\0'
   Just t  -> t
  where trad = lookup a (map swapTup (case lookup q qa_aLookup of Nothing -> []; Just n -> n;))

{- 
Donat un arbre (Question) i l'etiqueta a una resposta retorna el seguent arbre atractar.
-} 
seguentArbre :: Tree -> [Char] -> Tree
seguentArbre (Question t) a = case find (esResposta $ tNomAEtiq (fst t) a) (snd t) of
  Nothing -> Decision Indeterminat
  Just t  -> snd t

{-  
Genera un arbre a partir d'un conjunt de dades i l'imprimeix.
-}
generarArbre = do
  dadesAlFitcher <- readFile "agaricus-lepiota.data"
  let conjuntDeDades = (map parseLine . map (filter (/= ',')) . lines) dadesAlFitcher
  let arbreDecisions = makeTree conjuntDeDades []
  printt "" arbreDecisions

{- 
Genera un dialeg per predir la comestibilitat d'un bolet a partir d'un conjunt de 
dades.
-}
ferPrediccio = do
  dadesAlFitcher <- readFile "agaricus-lepiota.data"
  let conjuntDeDades = (map parseLine . map (filter (/= ',')) . lines) dadesAlFitcher
  let arbreDecisions = makeTree conjuntDeDades []
  interact $
    unlines .                           -- Joins all output lines
    map mostreArbre .                   -- Translates tree to strign
    takeWhile1 noEsDecisio .            -- Takes trees 'til decision 
    scanl seguentArbre arbreDecisions .
    lines

exits = exitWith  ExitSuccess
exite = exitWith (ExitFailure 1)
usage = putStrLn "Usage: ./dts [-h|-d|-p file]           \n\
                  \   -h        mostra aquest missatge   \n\
                  \   -a        genera l'atbre i el treu \
                                \ per sortida standard   \n\
                  \   -p        permet fer una prediccio \
                                \ a partir del dataset (per defecte) \n\
                  \ "

parse ["-h"] = usage        >> exits
parse ["-a"] = generarArbre >> exits
parse ["-p"] = ferPrediccio >> exits
parse     [] = ferPrediccio >> exits
parse  (x:_) = putStrLn ("Opcio desconeguda: " ++ x) >> usage >> exite

main = getArgs >>= parse