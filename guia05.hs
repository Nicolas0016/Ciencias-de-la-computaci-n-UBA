-- Ejercicio 1 

longitud :: [t]-> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud (xs)

ultimo :: [t]-> t
ultimo [x] = x
ultimo (x: xs) = ultimo xs

principio :: [t]-> [t]
principio [_] = []
principio (x:xs) = x : principio(xs)

reverso :: [t]-> [t]
reverso [] = []
reverso xs= (ultimo xs): (reverso (principio xs))


-- Ejercicio 2
pertenece :: Eq t => t -> [t] -> Bool  -- Añade la restricción Eq t
pertenece _ [] = False
pertenece buscar (x:xs)
    | x == buscar = True
    | otherwise = pertenece buscar xs

todosIguales :: Eq t => t-> [t]-> Bool
todosIguales x [item] = x == item 
todosIguales x (item: items)
    | x == item = True && todosIguales x (items)
    | otherwise = False 

todosDistintos:: Eq t => [t]-> Bool
todosDistintos  [] = True
todosDistintos (item: items)
    | pertenece item items = False
    | otherwise = True && todosDistintos items


hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos items = not (todosDistintos items)

 
quitarTodos :: (Eq t)=> t -> [t]-> [t]
quitarTodos _ [] = []
quitarTodos x (item: items)
    | x == item = quitarTodos x items
    | otherwise = item : quitarTodos x items

eliminarRepetidos :: (Eq t) => [t]-> [t]
eliminarRepetidos [] = []
eliminarRepetidos (item:items)
    | pertenece item items = eliminarRepetidos (items)
    | otherwise = item : eliminarRepetidos(items)


todosPertenecen :: Eq t => [t] -> [t] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) lista = pertenece x lista && todosPertenecen xs lista

mismosElementos :: Eq t => [t] -> [t] -> Bool
mismosElementos xs ys = 
    todosPertenecen xsLimpio ysLimpio && todosPertenecen ysLimpio xsLimpio
  where
    xsLimpio = eliminarRepetidos xs
    ysLimpio = eliminarRepetidos ys

capicua :: Eq t => [t] -> Bool
capicua xs = xs == reverso xs

--Ejercicio 3

sumatoria :: [Int] -> Int
sumatoria [x] = x
sumatoria (x:xs)= x + sumatoria xs


productoria :: [Int] -> Int
productoria [x] = x
productoria (x:xs) = x * productoria (xs)


xsCabeza :: [t]-> t
xsCabeza (x:_) = x
xsCola :: [t]->[t]
xsCola (_:x) = x
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs)
    | x < xsCabeza xs = maximo (xsCabeza xs: xsCola xs)
    | otherwise = maximo(x : xsCola xs)
    

sumaN :: Integer -> [Integer]-> [Integer]
sumaN n [x] = [n+x]
sumaN n (x:xs) = n+x : sumaN n xs

sumaElPrimero :: [Integer]-> [Integer]
sumaElPrimero xs = sumaN (xsCabeza xs) xs 

sumaElUltimo :: [Integer] -> [Integer]
sumaElUltimo xs = sumaN (ultimo xs) xs


pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
    | mod x 2 == 0 = x : pares(xs)
    | otherwise = pares(xs)

multiplosDeN :: Integer -> [Integer]-> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)
    | mod x n == 0 = x : multiplosDeN n (xs)
    | otherwise = multiplosDeN n (xs)

eliminarItem :: Eq a => a -> [a] -> [a]
eliminarItem _ [] = []
eliminarItem n (x: xs)
    | n == x = xs
    | otherwise = x: eliminarItem n xs

menor :: [Integer] -> Integer
menor [x] = x
menor (x:xs)
    | x > xsCabeza xs = menor (xsCabeza xs : xsCola xs)
    | otherwise = menor (x : xsCola xs )

ordenar:: [Integer]-> [Integer]
ordenar [x] = [x]
ordenar (xs) = menor xs :  ordenar (eliminarItem (menor xs) xs)


-- Ejercicio 4
type Text = [Char]
sacarBlancosRepetidos :: Text -> Text
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (a:b:xs)
    | a== b && a == ' ' = sacarBlancosRepetidos(b:xs)
    | otherwise = a:sacarBlancosRepetidos(b:xs)

sacarEspacioInicio (' ':xs) = xs
sacarEspacioInicio xs = xs

sacarEspacioFinal [' '] = []
sacarEspacioFinal [] = []
sacarEspacioFinal (x:xs)= x: sacarEspacioFinal(xs)


contarPalabrasFormat [x] = 1
contarPalabrasFormat (x:xs)
    | x == ' ' = 1 + contarPalabrasFormat xs
    | otherwise = contarPalabrasFormat xs  

contarPalabras ::Text -> Integer
contarPalabras (xs) = contarPalabrasFormat palabrasFormateadas
    where palabrasFormateadas = sacarEspacioFinal( sacarEspacioInicio (sacarBlancosRepetidos xs))

palabra :: Text->Text
palabra [] = []
palabra (' ' : _)= []
palabra (x:xs) = x:palabra(xs)

resto :: Text-> Text
resto [] = []
resto (' ':xs) = xs
resto(_:xs) = resto xs

palabras :: Text -> [Text]
palabras [] = []
palabras (' ':xs) = palabras xs
palabras xs = palabra xs : palabras (resto xs)

palabraMasLarga::Text -> Text
palabraMasLarga xs = palabraMasLargaAux (palabras (xs))

palabraMasLargaAux [x] = x
palabraMasLargaAux (a:b:xs)
    | longitud a > longitud b = palabraMasLargaAux (a:xs)
    | otherwise = palabraMasLargaAux (b:xs)

aplanarBlancos :: [Text] -> Text
aplanarBlancos [] = []
aplanarBlancos [x] = x
aplanarBlancos (x:xs) = x ++ " " ++ aplanarBlancos xs

blancos :: Integer ->Text
blancos 1 = [' ']
blancos n = ' ': blancos (n-1)

aplanarConNBlancos :: [Text] -> Integer -> Text
aplanarConNBlancos [] _ = []
aplanarConNBlancos [x] _ = x
aplanarConNBlancos (x:xs) n = x ++ (blancos n) ++ aplanarConNBlancos xs n
  
-- Ejercicio 5
sumatoriaLista :: Num t => [t]-> t
sumatoriaLista [x] = x
sumatoriaLista (x:xs) = x + sumatoriaLista xs
sumaAcumuladaDes :: Num t => [t] -> [t]
sumaAcumuladaDes [x] = [x]
sumaAcumuladaDes xs =  (sumatoriaLista (principio xs) + ultimo xs) : sumaAcumuladaDes (principio xs)

sumaAcumulada :: Num t => [t] -> [t]
sumaAcumulada xs = reverso (sumaAcumuladaDes xs)


----------------------------------------------------------
menorDivisorDesde :: Integral t => t -> t -> t
menorDivisorDesde n i 
    | mod n i == 0 = i
    | otherwise = menorDivisorDesde n (i+1)

menorDivisor :: Integral t => t -> t
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integral a => a -> Bool
esPrimo n = n == menorDivisor n

nEsimoPrimoDesde :: (Integral t1, Num t2, Eq t2) => t1 -> t2 -> t1
nEsimoPrimoDesde n i
    | i == 1 && esPrimo n = n
    | esPrimo n = nEsimoPrimoDesde (n+1) (i-1)
    | otherwise = nEsimoPrimoDesde (n+1) i

nEsimoPrimo :: (Num t2, Integral a, Eq t2) => t2 -> a
nEsimoPrimo 1 = 2
nEsimoPrimo n = nEsimoPrimoDesde 3 (n-1) 

descomponerPrimoDesde :: Integer -> Integer -> [Integer]
descomponerPrimoDesde n i
    | p > n          = []
    | mod n p == 0   = nEsimoPrimo i : descomponerPrimoDesde (div n p) i
    | otherwise      = descomponerPrimoDesde n (i + 1)
    where p = nEsimoPrimo i

descomponerPrimo :: Integer -> [Integer]
descomponerPrimo n = descomponerPrimoDesde n 1  

descomponerEnPrimos ::[Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = descomponerPrimo x : descomponerEnPrimos xs   


-- Ejercicio 6

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]


enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos x [] = False 
enLosContactos nombre ((contactName, _):xs)
    | nombre == contactName = True
    | otherwise = enLosContactos nombre xs

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto newContact [] = [newContact] 
agregarContacto (newName, newPhone) ((contactName, contactPhone):xs)
    | newName == contactName = (contactName, newPhone) : xs
    | otherwise = (contactName, contactPhone): agregarContacto (newName, newPhone) (xs)


eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto _ [] = []
eliminarContacto nombre ((contactName,phone):xs)
    | nombre == contactName = xs
    | otherwise = (contactName,phone) : eliminarContacto nombre xs
