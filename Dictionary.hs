module Dictionary(Dictionary(..), buildDict, search) where

data Dictionary = ROOTNODE [Dictionary] | LETTERNODE Char [Dictionary] | WORDNODE
  deriving Show

-- CONSTRUCCIÓN DEL DICCIONARIO --

buildDict :: [String] -> Dictionary
buildDict [] = ROOTNODE []
buildDict x = insert x (ROOTNODE []) 

-- Función insert --
insert :: [String] -> Dictionary -> Dictionary
insert [] dic = dic
insert (x:xs) dic = insert xs (insertInTree x dic)

-- Función insertInTree --
insertInTree :: String -> Dictionary -> Dictionary
insertInTree [] dic = dic
insertInTree (x:xs) (ROOTNODE[]) = ROOTNODE (insertChild x xs [])
insertInTree (x:xs) (ROOTNODE(cabeza:cola)) = ROOTNODE (insertChild x xs (cabeza:cola))
insertInTree (x:xs) (LETTERNODE nodo (cabeza:cola)) = LETTERNODE nodo (insertChild x xs (cabeza:cola))

-- Función insertChild --
insertChild :: Char -> [Char] -> [Dictionary] -> [Dictionary]
insertChild x [] [] = [LETTERNODE x [WORDNODE]]
insertChild x []  (WORDNODE:cola) = (WORDNODE):(insertChild x [] cola)
insertChild x [] ((LETTERNODE nodo (cabeza:cola)):hermanos)
        |x == nodo = if esWN (cabeza:cola) then ((LETTERNODE nodo (cabeza:cola)):hermanos) else ((LETTERNODE nodo (WORDNODE:(cabeza:cola))):hermanos)
        |x < nodo = ((LETTERNODE x [WORDNODE]):(LETTERNODE nodo (cabeza:cola)):hermanos)
        |otherwise = (LETTERNODE nodo (cabeza:cola)):(insertChild x [] hermanos)
insertChild x (y:ys) [] = [LETTERNODE x (insertChild y ys [])]
insertChild x (y:ys) (WORDNODE:cola) = (WORDNODE):(insertChild x (y:ys) cola)
insertChild x (y:ys) ((LETTERNODE nodo (cabeza:cola)):hermanos)
        |x == nodo = (LETTERNODE x (insertChild y ys (cabeza:cola)):hermanos)
        |x < nodo = ((LETTERNODE x (insertChild y ys [])):(LETTERNODE nodo (cabeza:cola)):hermanos)
        |otherwise = ((LETTERNODE nodo (cabeza:cola)):insertChild x (y:ys) hermanos)

--FUNCIONES AUXILIARES PARA LA CONSTRUCCIÓN--

esWN::[Dictionary] -> Bool
esWN (LETTERNODE nodo (cabeza:cola):hermanos) = False
esWN (WORDNODE:cola) = True


-- BÚSQUEDA EN EL DICCIONARIO --

-- Función search --
search :: String -> Dictionary -> [String] 
search [] diccionario = []
search x diccionario = searchInTree x "" diccionario

-- Función searchInTree --
searchInTree :: String -> String -> Dictionary -> [String]
searchInTree [] palabra (LETTERNODE nodo (cabeza:cola)) 
        |esWN (cabeza:cola) = [palabra]
        |otherwise = []
searchInTree caracteres [] diccionario = searchChild caracteres "" diccionario
searchInTree caracteres palabra (LETTERNODE nodo  []) = []    
searchInTree caracteres palabra (LETTERNODE nodo (cabeza:cola)) = (searchChild caracteres palabra cabeza)++(searchInTree caracteres palabra (LETTERNODE nodo (cola)))

-- Función searchChild --
searchChild :: String -> String -> Dictionary -> [String]
searchChild caracteres palabra (ROOTNODE (cabeza:cola)) = searchChildAuxNodo caracteres palabra [] (cabeza:cola)
searchChild caracteres palabra (LETTERNODE nodo (cabeza:cola))= searchChildAuxHijos caracteres palabra [] (LETTERNODE nodo (cabeza:cola))
searchChild caracteres palabra (WORDNODE) = [palabra]

--FUNCIONES AUXILIARES PARA LA BÚSQUEDA--

-- Función searchChildAuxHijos --
searchChildAuxHijos :: String -> String -> String -> Dictionary -> [String]
searchChildAuxHijos [] _ _ (LETTERNODE nodo (cabeza:cola)) = []
searchChildAuxHijos [] palabra _ (WORDNODE)= [palabra]
searchChildAuxHijos (x:xs) palabra acumulador (LETTERNODE nodo (cabeza:cola))
        |x == nodo = searchChildAuxNodo (xs ++ acumulador) (palabra++[x]) [] (cabeza:cola)
        |otherwise = searchChildAuxHijos xs palabra (acumulador++[x]) (LETTERNODE nodo (cabeza:cola))
searchChildAuxHijos (x:xs) palabra acumulador (WORDNODE)
        |null(palabra) = [] 
        |otherwise = [palabra]  

-- Función searchChildAuxNodo --
searchChildAuxNodo :: String -> String -> String -> [Dictionary] -> [String]
searchChildAuxNodo _ _ _ [] = []
searchChildAuxNodo caracteres palabra acumulador (cabeza:cola) = (searchChildAuxHijos caracteres palabra acumulador cabeza) ++ (searchChildAuxNodo caracteres palabra acumulador cola)