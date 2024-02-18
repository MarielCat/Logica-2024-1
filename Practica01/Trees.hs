{-
- Lógica Computacional 2024-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: Marco Vladimir Lemus Yáñez
- Ayudante: Naomi Itzel Reyes Granados
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: José Ricardo Desales Santos
- Practica 1: Recordando Haskell. Árboles
- Integrantes:
- Arias Villaroel Alejandra Valentina
- Del Monte Ortega Maryam Michelle
- Martínez Mejía Eduardo
- Monroi Romero Sahara Mariel
-}

module Trees where

import Data.List

data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Regresa el número de nodos de un árbol.
nNodes :: BTree a -> Int
nNodes Void = 0 --Si el arbol es vacio, tiene 0 nodos
nNodes (Node n h1 h2) = 1 + nNodes h1 + nNodes h2 -- Cuenta el primer nodo y luego cada hijo de este de forma recursiva

-- | Regresa el número de hojas de un árbol.
nLeaves :: BTree a -> Int
nLeaves Void = 0 --Si el arbol es vacio, no tiene hojas
nLeaves (Node n Void Void) = 1 --Si el nodo no tiene hijos, entonces es una hoja
nLeaves (Node n h1 h2) = nLeaves h1 + nLeaves h2 --Recorre los hijos de cada nodo recursivamente hasta encontrar una hoja y sumar 1

-- | Regresa el número de nodos internos de un árbol.
nni :: BTree a -> Int
nni Void = 0 --Si el arbol es vacio, no tiene nodos
nni (Node n Void Void) = 0 --Si el nodo es hoja, entonces no se cuenta
nni (Node n h1 h2) = 1 + nni h1 + nni h2 --Cuenta el primer nodo y luego cada hijo de este de forma recursiva excepto las hojas

-- | Nos dice si un elemento está contenido en un árbol ordenado.
contains :: (Ord a, Eq a) => a -> BTree a -> Bool
contains a Void = False --Si el arbol es vacio, el elemento no esta
contains a (Node n h1 h2) = a == n || contains a h1 || contains a h2 --Busca si el elemento esta en el nodo. SI no se encuentra se busca recursivamente en sus hijos.

-- | Recorrido inorder.
inorder :: BTree a -> [a]
inorder Void = [] --Si el arbol es vacio, regresa una lista vacia
inorder (Node n h1 h2) = inorder h1 ++ [n] ++ inorder h2 --Agrega primero los hijos izquierdos, luego el nodo padre y al ultimo los hijos derechos a una lista

-- | Recorrido preorder.
preorder :: BTree a -> [a]
preorder Void = [] --Si el arbol es vacio, regresa una lista vacia
preorder (Node n h1 h2) = [n] ++ preorder h1 ++ preorder h2 --Agrega primero el nodo padre, luego los hijos izquierdos y al ultimo los derechos en una lista

-- | Recorrido postorder.
postorder :: BTree a -> [a]
postorder Void = [] --Si el arbol es vacio, regresa una lista vacia
postorder (Node n h1 h2) = postorder h1 ++ postorder h2 ++ [n] -- Agrega primero los hijos izquierdos, luego los derechos y al ultimo al nodo padre en una lista

-- | Agrega un elemento a un árbol binario de manera ordenada.
add :: (Ord a) => a -> BTree a -> BTree a
add a Void = Node a Void Void --Si el arbol es vacio, introduce el elemento como cabeza
add a (Node n h1 h2) 
  | a < n =  Node n (add a h1) h2 --Si el elemento nuevo es menor al nodo,  lo agrega al subarbol izquierdo
  | a > n = Node n h1 (add a h2) --Si el elemento nuevo es mayor al nodo, lo agrega al subarbol derecho
  | otherwise = Node n h1 h2 -- En caso de que el elemento nuevo sea igual al nodo, significa que el elemento ya existe y no se agrega, regresando la misma lista


-- | Pasa una lista a un árb ol binario de forma ordenada.
fromList :: (Ord a) => [a] -> BTree a -> BTree a
fromList [] t = t --Si la lista a agregar es vacia, se regresa el mismo arbol
fromList (x:xs) Void = fromList xs (add x Void) --Si el arbol es vacio, se crea un nodo inicial con el primer elemento de la lista y recursivamente se agrega el resto de la lista
fromList (x:xs) t = fromList xs (add x t) -- Se agrega el primer elemento de la lista y recursivamente se agrega el resto de la lista


--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

v :: BTree Int
v = Void

test :: BTree Int
test = (Node 10
         (Node 5
           (Node 2
             (Node 1 v v)
             (Node 3 v v))
           (Node 8
             (Node 6 v v)
             (Node 9 v v)))
         (Node 15
           (Node 12
             (Node 11 v v)
             (Node 13 v v))
           (Node 17
             (Node 16 v v)
             (Node 18 v v))))

nn1 = nNodes test
-- Regresa: 15

nLeaves1 = nLeaves test
-- Regresa: 8

nni1 = nni test
-- Regresa: 7

contains1 = contains 0 test
-- Regresa: False

contains2 = contains 13 test
-- Regresa: True

inorder1 = inorder test
-- Regresa: [1,2,3,5,6,8,9,10,11,12,13,15,16,17,18]

preorder1 = preorder test
-- Regresa: [10,5,2,1,3,8,6,9,15,12,11,13,17,16,18]

postorder1 = postorder test
-- Regresa: [1,3,2,6,9,8,5,11,13,12,16,18,17,15,10]

add1 = add 0 test
-- Regresa:
-- Node 10
--   (Node 5
--     (Node 2
--      (Node 1
--       (Node 0 Void Void)
--       Void)
--      (Node 3 Void Void))
--     (Node 8
--       (Node 6 Void Void)
--       (Node 9 Void Void)))
--   (Node 15
--     (Node 12
--      (Node 11 Void Void)
--      (Node 13 Void Void))
--     (Node 17
--       (Node 16 Void Void)
--       (Node 18 Void Void)))

add2 = add 20 test
-- Regresa:
-- Node 10
--   (Node 5
--     (Node 2
--       (Node 1 Void Void)
--       (Node 3 Void Void))
--     (Node 8
--       (Node 6 Void Void)
--       (Node 9 Void Void)))
--   (Node 15
--     (Node 12
--       (Node 11 Void Void)
--       (Node 13 Void Void))
--     (Node 17
--       (Node 16 Void Void)
--       (Node 18 Void
--         (Node 20 Void Void))))

add3 = add 7 v
-- Regresa: Node 7 Void Void

fromList1 = fromList [5,3,8,1,2,6,9,7,10,4] Void
-- Regresa:
-- Node 5
--   (Node 3
--     (Node 1 Void
--       (Node 2 Void Void))
--     (Node 4 Void Void))
--   (Node 8
--     (Node 6 Void
--       (Node 7 Void Void))
--     (Node 9 Void
--       (Node 10 Void Void)))
