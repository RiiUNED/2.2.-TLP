module SparseArray where

import Data.Tree

data Value a = Null | Value a
  deriving (Eq,Read,Show)
data SparseArray a = Vacio | Nodo (Value a) (SparseArray a) (SparseArray a)
  deriving (Eq,Read,Show)

{- Función num2bin: recibe un Int y devuelve una lista 
con los dígitos de su representación en binario -}
num2bin :: Int -> [Bool]
num2bin 0 = [False]
num2bin n = num2binAux n
  where num2binAux :: Int -> [Bool]
        num2binAux 0 = []
        num2binAux n = (num2binAux (div n 2))++[(mod n 2)==1]

-- Funcion newSparseArray: devuelve un SparseArray vacio --
newSparseArray :: Eq a => SparseArray a
newSparseArray = Vacio

-------------- dibujaArbol ------------------------
--Funciones para mostrar datos
toDataTree :: Show a => SparseArray a -> Tree String
toDataTree Vacio = Node "Vacio" []
toDataTree (Nodo n i d) = Node (show n) [toDataTree i, toDataTree d]

dibujaArbol :: Show a => SparseArray a -> IO ()
dibujaArbol arbol = putStr (drawTree (toDataTree (arbol)))

-------------- Set ------------------------
--Funciones auxiliares Set
--Datos
setData :: Eq a => SparseArray a -> a -> SparseArray a
setData Vacio dato = Nodo (Value dato) (Vacio) (Vacio)
setData (Nodo (n) (l) (r)) dato = Nodo (Value dato) (l) (r)
--Arbol izquierdo 
setLeftTree :: Eq a => SparseArray a -> SparseArray a -> SparseArray a
setLeftTree Vacio Vacio = Vacio 
setLeftTree Vacio arbol = Nodo (Null) (arbol) (Vacio)
setLeftTree (Nodo (n) (l) (r)) arbol = Nodo (n) (arbol) (r)
--Arbol derecho 
setRightTree :: Eq a => SparseArray a -> SparseArray a -> SparseArray a
setRightTree Vacio Vacio = Vacio 
setRightTree Vacio arbol = Nodo (Null) (Vacio) (arbol)
setRightTree (Nodo (n) (l) (r)) arbol = Nodo (n) (l) (arbol)

--Funcion auxiliar extender el arbol
spreadTree :: Eq a => [Bool] -> SparseArray a -> a -> SparseArray a
spreadTree [] arbol dato = setData arbol dato
spreadTree (x:xs) arbol dato
    | arbol == Vacio = generateTree (x:xs) dato
    | x == True = setRightTree arbol (spreadTree (xs) (getRightTree arbol) dato)  
    | x == False = setLeftTree arbol (spreadTree (xs) (getLeftTree arbol) dato)

--genera arboles nuevos 
generateTree :: Eq a => [Bool] -> a -> SparseArray a
generateTree [] dato = setData newSparseArray dato
generateTree (x:xs) dato
    | x == True = setRightTree newSparseArray (generateTree (xs) dato)
    | x == False = setLeftTree newSparseArray (generateTree (xs) dato)
 
{- Funcion set: recibe un SparseArray, una posición y un elemento y cambia el valor del SparseArray de dicha posición -}
set :: Eq a => SparseArray a -> Int -> a -> SparseArray a
set arbol posicion dato = spreadTree (num2bin posicion) arbol dato

-------------- Get ------------------------
--Funciones auxiliares get
--Datos
getData :: Eq a => SparseArray a -> Value a
getData (Nodo n leftTree rightTree) = n
getData Vacio = Null
--Arbol izquierdo
getLeftTree :: Eq a => SparseArray a -> SparseArray a
getLeftTree (Nodo (n) (leftTree) (rightTree)) = leftTree
--Arbol derecho
getRightTree :: Eq a => SparseArray a -> SparseArray a
getRightTree (Nodo (n) (leftTree) (rightTree)) = rightTree

--Recorre el arbol
runTree :: Eq a => [Bool] -> SparseArray a -> (Value a)
runTree _ Vacio =Null
runTree [] (Nodo n leftTree rightTree) = n
runTree (True:xs) (Nodo n leftTree rightTree) = runTree xs rightTree 
runTree (False:xs) (Nodo n leftTree rightTree) = runTree xs leftTree

-- Función get: recibe un SparseArray y una posición y devuelve el elemento del SparseArray en dicha posición --
get :: Eq a => SparseArray a -> Int -> (Value a)
get Vacio posicion =Null
get arbol posicion =runTree (num2bin posicion) arbol

-------------- Delete ------------------------
--Funciones auxiliares delete
-- Test de permanencia de los nodos tras delete
presenceTest :: Eq a => SparseArray a -> SparseArray a
presenceTest Vacio = Vacio
presenceTest (Nodo Null (Vacio) (Vacio)) = Vacio
presenceTest (Nodo n (rT) (lT)) = Nodo n (rT) (lT)

-- borrar nodo
setNull :: Eq a => SparseArray a -> SparseArray a
setNull Vacio = Vacio
setNull (Nodo n (lT) (rT)) = (Nodo Null (lT) (rT))

deletingNodes :: Eq a => [Bool] -> SparseArray a -> SparseArray a
deletingNodes [] arbol = presenceTest (setNull arbol)
deletingNodes _ Vacio = Vacio
deletingNodes (x:xs) arbol
    | x == True = setRightTree arbol (presenceTest (deletingNodes (xs) (getRightTree arbol)))
    | x == False = setLeftTree arbol (presenceTest (deletingNodes (xs) (getLeftTree arbol)))

{- Funcion delete: recibe un SparseArray y una posicion y devuelve el SparseArray resultado de eliminar dicha posicion -}
-- Tambien elimina todos los nodos vacios que resulten de la eliminacion 
delete :: Eq a => SparseArray a -> Int -> SparseArray a
delete t pos = presenceTest (deletingNodes (num2bin pos) t)
