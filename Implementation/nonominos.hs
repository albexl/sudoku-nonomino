import Data.List

---------------------Definicion de tipos de datos utiles----------------------------------------------

--clase nonomino (una lista de (posicion, valor) y un identificador)
data Nonomino = Nonomino{
    positions :: [ ( (Int, Int), Int) ],
    idx :: Int
}deriving (Show, Read)






----------------------Validacion del Sudoku en funcion de los Nonominos-----------------------------

--trasladar la lista de posiciones de un nonomino dado un vector(OK)
traslateList :: [((Int, Int), Int)] -> (Int, Int) -> [(Int, Int)]
traslateList [] _ = []                                                                                  --OK
traslateList (x : xs) y = [( (fst (fst x)) + (fst y), (snd (fst x)) + (snd y) )] ++ (traslateList xs y) --OK

--comprueba si este orden de nonominos puede formar un sudoku valido (a este se le pasa
--la lista de posiciones aun no cubiertas) (OK)
checkValidity :: [Nonomino] -> [(Int, Int)] -> Bool
checkValidity [] y = if length y == 0 then True else False                                         --OK
checkValidity (x : xs) y = checkValidity xs (y Data.List.\\ (traslateList (positions x) (y!!0) ) )   --OK

--devuelve todas las posiciones de un conjunto de nonominos(OK)
joinPositions :: [Nonomino] -> [((Int, Int), Int)]
joinPositions [] = []                                           --OK
joinPositions (x : xs) = (positions x) ++ (joinPositions xs)    --OK

--comprueba la validez de dos componentes(OK)
componentCheck :: Int -> Int -> Int -> Int -> Bool
componentCheck x y w z = if x /= y then True else if w /= z then True else if w == 0 then True else False  --OK

--comprueba si dos posiciones son compatibles(OK)
compatible :: ((Int, Int), Int) -> ((Int, Int), Int) -> Bool
compatible x y = (componentCheck (fst (fst x) ) (fst (fst y) ) (snd x) (snd y) ) && (componentCheck (snd (fst x) ) (snd (fst y) ) (snd x) (snd y) ) --OK

--comprueba colisiones entre una posicion y un conjunto de posiciones(OK)
checkAgainst :: ((Int, Int), Int) -> [((Int, Int), Int)] -> Bool
checkAgainst x [] = True                                            --OK
checkAgainst x (y : ys) = (compatible x y) && (checkAgainst x ys)   --OK

--comprueba colisiones entre dos conjuntos de posiciones(OK)
checkColisions :: [((Int, Int), Int)] -> [((Int, Int), Int)] -> Bool
checkColisions [] x = True                                              --OK
checkColisions (x : xs) y = (checkAgainst x y) && (checkColisions xs y) --OK

--comprueba colisiones en filas y columnas(OK)
check :: [Nonomino] -> Bool
check (x : []) = True                                                                  --OK
check (x : xs) = (checkColisions (positions x) (joinPositions xs)) && (check xs)       --OK

--comprueba si este orden de nonominos puede formar un sudoku valido(OK)
fit :: [Nonomino] -> Bool
fit n = (checkValidity n ([(x, y) | x <- [0..8], y <- [0..8] ]) )  && (check (getItTraslated n ([ (x, y) | x <- [0..8], y <- [0..8] ]) ) ) 

--devuelve un nonomino trasladado por un vector(OK)
traslateNonomino :: Nonomino -> (Int, Int) -> Nonomino
traslateNonomino x y = Nonomino{ positions = ([ (( ( (fst (fst z)) + (fst y) ), ( (snd (fst z)) + (snd y) ) ), (snd z) ) | z <- positions x]), idx = idx x} --OK

--devuelve una lista de nonominos trasladado por un vector(OK)
getItTraslated :: [Nonomino] -> [(Int, Int)] -> [Nonomino]
getItTraslated [] y = []                                       --OK
getItTraslated (x : xs) y = [traslateNonomino x (y!!0)] ++ (getItTraslated xs (y Data.List.\\ (traslateList (positions x) (y!!0) ) ))   --OK


--devuelve una configuracion de nonominos que cumpla con un predicado(devuelve un par: la configuracion valida
--de los nonominos y la solucion del sudoku con esta configuracion)(OK)
some :: ([Nonomino] -> Bool) -> [[Nonomino]] -> ( [Nonomino], [((Int, Int), Int)] )
some f [] = ([], [])
some f (x : xs) = case f x of
    True -> if ( (length result) > 0 ) then (wellPositioned, result) else some f xs
    False -> some f xs
    where
        allPositions = ([ (x, y) | x <- [0..8], y <- [0..8] ])
        wellPositioned = (getItTraslated x allPositions)
        result = (solveSudoku wellPositioned)


--devuelve un orden valido para ubicar los nonominos y la solucion del sudoku resultante en este orden 
--o [] en caso de no existir ningun orden que genere un sudoku valido
solution :: [Nonomino] -> ([Nonomino], [((Int, Int), Int)])
solution x = some (fit) (permutations x)







------------------------------Resolver el Sudoku---------------------------

--resuelve el sudoku dado una lista de nonominos bien posicionados(OK)
solveSudoku :: [Nonomino] -> [((Int, Int), Int)]
solveSudoku x = (sudokuSolver ( getFields (x!!0) ) (x!!0) (tail x) (1) (getSet x) )

--devuelve los campos de un nonomino(OK)
getFields :: Nonomino -> ( [ ( (Int, Int ), Int ) ], Int)
getFields x = ( (positions (x) ), (idx (x) ) )

--resuelve el sudoku(metodo recursivo cuyos parametros son: nonomino actual,
--lista de nonominos que restan por resolver, numero que voy a intentar poner en esta posicion del nonomino actual,
--lista de posiciones con un numero puesto)
sudokuSolver :: ( [( ( Int, Int ), Int )], Int) -> Nonomino -> [Nonomino] -> Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
sudokuSolver ([], _) _ [] _ on = on
sudokuSolver ([], _) him (h : t) num on = sudokuSolver (getFields (h)) (h) (t) (1) (on)
sudokuSolver ( ( ( (x, y), val) : t), idx) him nonos num on | val > 0 = sudokuSolver (t, idx) (him) (nonos) (1) (on)
                                                      | num >= 10 = []
                                                      | otherwise = if (( checkNonomino (positions him) (num) (on) ) && (checkRow (x) (on) (num) (0)) && (checkCol (y) (on) (num) (0)) && ( (length (result) ) > 0)) then result
                                                                    else sudokuSolver ( ( ( (x, y), val) : t), idx) (him) (nonos) (num + 1) (on) 
                                                        where
                                                            new_on = ( [( (x, y), num)] ++ on)
                                                            result = sudokuSolver ((t, idx)) (him) (nonos) (1) (new_on)

--devuelve todos los pares (x, y) de una lista de la forma ((x, y), val)
getPairs :: [ ((Int, Int), Int)] -> [(Int, Int)]
getPairs [] = []
getPairs (x : xs) = ((fst x) : (getPairs xs))

--comprueba si en un nonomino se puede ubicar un valor dado
checkNonomino :: [ ( (Int, Int), Int) ] -> Int -> [((Int, Int), Int)] -> Bool
checkNonomino list num on = length(newList) == 0
                         where
                            newList = [ ((a, b), c) | ((a, b), c) <- on, c == num, (a, b) `elem` (getPairs list)]

--comprueba si en un fila se puede ubicar un valor dado
checkRow :: Int -> [((Int, Int), Int)] -> Int -> Int -> Bool
checkRow row on num want = (length(list) == want)
                            where
                                list = [c | ((a, b), c) <- on, a == row, c == num]

--comprueba si en un columna se puede ubicar un valor dado
checkCol :: Int -> [((Int, Int), Int)] -> Int -> Int -> Bool
checkCol col on num want = (length(list) == want)
                            where
                                list = [c | ((a, b), c) <- on, b == col, c == num]

--devuelve todos las posiciones con algun numero puesto(OK)
notZero :: [((Int, Int), Int)] -> [((Int, Int), Int)]
notZero [] = []
notZero (x : xs) = if (snd x == 0) then (notZero xs) else ([x] ++ (notZero xs))
    
--obtiene las posiciones con algun numero puesto(OK)
getSet :: [Nonomino] -> [((Int, Int), Int)]
getSet [] = []
getSet (x : xs) = (notZero (positions x)) ++ (getSet xs)





--------------------------------Metodos de Input/Output--------------------------------------

--lectura de las componentes de un nonomino(OK)
readAsTuple :: String -> ([((Int, Int), Int)], Int)
readAsTuple s = read s :: ([((Int, Int), Int)], Int)

--lectura de un nonomino(OK)
readNonomino :: String -> Nonomino
readNonomino s = Nonomino{ positions = fst (line) , idx = snd (line) } 
                 where line = readAsTuple s

--devuelve el id del nonomino al que pertenece la posicion (x, y)
getNonominoId :: [Nonomino] -> (Int, Int) -> Int
getNonominoId (nono : ts) (x, y) = if (x, y) `elem` ( map fst (positions nono) ) then (idx nono) else getNonominoId ts (x, y)
getNonominoId _ _ = 0

--devuelve el valor asociado a la posicion (x, y) dado el conjunto de todas las posiciones
getVal :: [((Int, Int), Int)] -> (Int, Int) -> Int
getVal ( ( (v, w), z) : t) (x, y) = if x == v && y == w then z else getVal t (x, y)
getVal _ _ = 0

--devuelve una matriz con los ids asignados a cada posicion
convertToIds :: [Nonomino] -> [[Int]]
convertToIds nonominos = [ [ getNonominoId nonominos (x, y) | y <- [0 .. 8] ] | x <- [0 .. 8]]

--devuelve una matriz con los valores asignados a cada posicion
convertToVals :: [((Int, Int), Int)] -> [[Int]]
convertToVals vals = [ [ getVal vals (x, y) | y <- [0..8] ] | x <- [0..8] ]

--imprime una lista de listas(matriz) ([[Int]])
printList [x] = print x
printList ( (x) : xs ) = do
    print x
    printList xs 




-------------------------------------Main--------------------------------------------------------
main = do
    inp <- readFile "input.in"              --leer el fichero con el conjunto de nonominos
    let x = map readNonomino (lines inp)    --listarlos como los nonominos definidos en la clase nonomino

    let sol = solution x                    --devuelve la solucion del sudoku

    let setting = fst sol                   --la primera componente es la distribucion de los nonominos
    let filled = snd sol                    --la segunda componente es el sudoku resuelto

    print "sudoku distribution:"                        
    printList (convertToIds (setting) )    --imprime la distribucion de los nonominos o un sudoku vacio en caso de que no haya solucion
    
    print "solution:"
    printList (convertToVals (filled) )    --imprime el sudoku resuelto o un sudoku vacio en caso de que no haya solucion