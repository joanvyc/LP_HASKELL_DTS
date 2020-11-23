
# Decision Tree amb Haskell

Aquest projecte forma part de l'acrivitat academica de l'assignatura de Lenguatges de 
Programacio (LP) de la Facultat d'Informatica de Barcelona (FIB) i consisteix en la
implementacio dels algosrismes de creacio i navegacio d'un arbre de decisions, amb el 
llenguatge de programacio Haskell. 

## Instalacio / Compilacio

Per utilitzar aquest programa simplement s'ha de compilar mitjancant la commanda:
```bash
ghc dts.hs
```

## Usage

Per poder utilitzar aquest programa es necessari tenir al directori d'execuccio el 
fitxer agaricus-lepiota.data (que es pot descarregar en aquest [enllac](https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data)).
Finalment simplement s'executa el binari amb una de les seguents opcions.

Usage: ./dts [-h|-d|-p file]           
   -h        mostra aquest missatge   
   -a        genera l'atbre i el treu  per sortida standard   
   -p        permet fer una prediccio  a partir del dataset 