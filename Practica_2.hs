-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------
 
import Data.List 
import System.IO
 
-- ---------------------------------------------------------------------
-- § Implementación del juego                                         --
-- ---------------------------------------------------------------------
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la constante
--    profundidadDeBusqueda :: Int
-- tal que profundidadDeBusqueda es el máximo nivel de profundidad del
-- árbol de análisis del juego. Por defecto es 6. 
-- 
-- Nota: Cuanto mayor sea la profundidadDeBusqueda, mejor juega el
-- computador pero su velocidad es menor. 
-- ---------------------------------------------------------------------
 
profundidadDeBusqueda :: Int
profundidadDeBusqueda = 10
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Las posiciones del tablero se numeran como se indica a
-- continuación: 
--    1|2|3
--    -+-+-
--    4|5|6
--    -+-+-
--    7|8|9
-- 
-- Definir el tipo de dato Posicion para representar una posición del
-- tablero. Cada posición es un entero del 1 a 9.
-- ---------------------------------------------------------------------
 
type Posicion = Int
 
-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir el tipo de datos Posiciones para representar
-- listas de posiciones.  
-- ---------------------------------------------------------------------
 
type Posiciones = [Posicion]
 
-- ---------------------------------------------------------------------
-- Ejercicio 4. En el juego participan dos jugadores. El jugador X es el
-- que comienza el juego y el otro jugador es el O.
-- 
-- Definir el tipo de datos Tablero para representar los tableros. El
-- tablero de la forma (Tab xs os) representa un tablero donde xs es
-- la lista de las posiciones donde están colocadas las fichas del
-- primer jugador y os la del segundo jugador. 
-- ---------------------------------------------------------------------
 
data Tablero = Tab Posiciones Posiciones 
               deriving Show
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la constante
--    tableroInicial :: Tablero
-- para representar el tablero inicial en el no hay colocada ninguna
-- ficha. 
-- ---------------------------------------------------------------------
 
tableroInicial :: Tablero
tableroInicial = Tab [] []
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    turnoDeX :: Tablero -> Bool
-- tal que (turnoDeX t) se verifica si en el tablero t le toca mover al
-- jugador X. Como X es el que inicia el juego, su turno es cuando son
-- iguales el número de X y de O colocadas. Por ejemplo, 
--    turnoDeX (Tab []  [])   ==  True
--    turnoDeX (Tab [3] [])   ==  False
--    turnoDeX (Tab [3] [7])  ==  True
-- ---------------------------------------------------------------------
 
turnoDeX :: Tablero -> Bool
turnoDeX (Tab xs os) = 
    length xs == length os
 
-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    pone :: Tablero -> Posicion -> Tablero
-- tal que (pone t p) es el tablero obtenido poniendo en la posición
-- p del tablero t una ficha del jugador al que le corresponde
-- colocar. Por ejemplo,
--    pone (Tab []    [])  3  ==  Tab [3] []
--    pone (Tab [3]   [])  5  ==  Tab [3] [5]
--    pone (Tab [3]   [5]) 8  ==  Tab [8,3] [5]
--    pone (Tab [8,3] [5]) 4  ==  Tab [8,3] [4,5]
-- ---------------------------------------------------------------------
 
pone :: Tablero -> Posicion -> Tablero
pone (Tab xs os) p | turnoDeX (Tab xs os) = Tab (p:xs) os
                   | otherwise            = Tab xs (p:os)
 
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    completo :: Tablero -> Bool
-- tal que (completo t) se verifica si el tablero t está completo; es
-- decir, se han colocado las 9 fichas. Por ejemplo,
--    completo (Tab [3,9,5,8,4] [1,7,6,2])  ==  True
--    completo (Tab [3,9,5,8,4] [1,7,6])    ==  False
-- ---------------------------------------------------------------------
 
completo :: Tablero -> Bool
completo (Tab xs os) = length xs + length os == 9
 
-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    subconjunto :: Posiciones -> Posiciones -> Bool
-- tal que (subconjunto s1 s2) se verifica si s1 es un subconjunto de
-- s2. Por ejemplo,
--    subconjunto [3,2,5] [6,2,4,5,3]  ==  True
--    subconjunto [3,2,5] [6,2,4,3]    ==  False
-- ---------------------------------------------------------------------
 
subconjunto :: Posiciones -> Posiciones -> Bool
subconjunto s1 s2 = all (`elem` s2) s1
 
-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    tieneLinea :: Posiciones -> Bool
-- tal que (tieneLinea ps) se verifica si la lista de posiciones ps 
-- contiene una línea horizontal, vertical o diagonal. Por ejemplo,
--    tieneLinea [2,5,3,6,4]  ==  True
--    tieneLinea [2,5,3,6]  ==  False
-- ---------------------------------------------------------------------
 
tieneLinea :: Posiciones -> Bool
tieneLinea ps = 
    subconjunto [1,2,3] ps ||subconjunto [4,5,6] ps ||subconjunto [7,8,9] ps ||
    subconjunto [1,4,7] ps ||subconjunto [2,5,8] ps ||subconjunto [3,6,9] ps ||
    subconjunto [1,5,9] ps ||subconjunto [3,5,7] ps
 
-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    tieneGanador :: Tablero -> Bool
-- tal que (tieneGanador t) se verifica si el tablero t tiene un
-- ganador; es decir, alguno de los dos jugadores ha conseguido una
-- línea. Por ejemplo,
--    tieneGanador (Tab [2,5,3,6] [1,7,8,9])  ==  True
--    tieneGanador (Tab [2,5,3,9] [1,6,8,7])  ==  False
-- ---------------------------------------------------------------------
 
tieneGanador :: Tablero -> Bool
tieneGanador (Tab xs os) = tieneLinea xs || tieneLinea os
 
-- ---------------------------------------------------------------------
-- § Construcción del árbol de juego                                  --
-- ---------------------------------------------------------------------
 
-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir el tipo de datos Arbol para representa los
-- árboles compuestos por nodos con una lista de hijos. Por ejemplo,
--    ghci> :type Nodo 1 [Nodo 2 [Nodo 4 []], Nodo 3 []]
--    Nodo 1 [Nodo 2 [Nodo 4 []], Nodo 3 []] :: Num a => Arbol a
-- ---------------------------------------------------------------------
 
data Arbol a = Nodo a [Arbol a]
 
-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    muestraArbol :: Show t => Arbol t -> String
-- tal que (muestraArbol t) es una cadena que representa el árbol
-- t para una mejor visualización. Hacer la clase Arbol una
-- instancia de Show definiendo show como muestraArbol. Por
-- ejemplo,
--    ghci> muestraArbol (Nodo 1 [Nodo 2 [Nodo 4 []], Nodo 3 []])
--    "1\n  2\n    4\n  3\n"
--    ghci> Nodo 1 [Nodo 2 [Nodo 4 []], Nodo 3 []]
--    1
--      2
--        4
--      3
-- ---------------------------------------------------------------------
 
muestraArbol (Nodo x xs) = 
    show x ++ '\n' : (unlines . map ("  "++) . concatMap (lines . show)) xs
 
instance Show a => Show (Arbol a) where
  show = muestraArbol
 
 
-- En la siguiente sesión se muestra el comportamiento de muestraArbol.
--    ghci> show 1
--    "1"
--    ghci> concatMap (lines . show) [Nodo 2 [Nodo 4 []], Nodo 3 []]
--    ["2","  4","3"]
--    ghci> map ("  "++) ["2","  4","3"]
--    ["  2","    4","  3"]
--    ghci> unlines ["  2","    4","  3"]
--    "  2\n    4\n  3\n"
--    ghci> "1" ++ '\n' : "  2\n    4\n  3\n"
--    "1\n  2\n    4\n  3\n"
 
-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    posicionesLibres :: Tablero -> Posiciones
-- tal que (posicionesLibres t) es la lista de las posiciones libres del
-- tablero t. Por ejemplo,
--    posicionesLibres (Tab [3,2] [1,7])  ==  [4,5,6,8,9]
-- ---------------------------------------------------------------------
 
posicionesLibres :: Tablero -> Posiciones
posicionesLibres (Tab xs os) = [1..9] \\ (xs++os)
 
-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    siguientesTableros :: Tablero -> [Tablero]
-- tal que (siguientesTableros t) es la lista de tableros obtenidos
-- colocando una pieza en cada una de las posiciones libres de t. Por
-- ejemplo,
--    ghci> tableroInicial
--    Tab [] []
--    ghci> siguientesTableros tableroInicial
--    [Tab [1] [], Tab [2] [], Tab [3] [], Tab [4] [], Tab [5] [],
--     Tab [6] [], Tab [7] [], Tab [8] [], Tab [9] []]
--    ghci> siguientesTableros (Tab [1] [])
--    [Tab [1] [2], Tab [1] [3], Tab [1] [4], Tab [1] [5],
--     Tab [1] [6], Tab [1] [7], Tab [1] [8], Tab [1] [9]]
--    ghci> siguientesTableros (Tab [1] [2])
--    [Tab [3,1] [2], Tab [4,1] [2], Tab [5,1] [2], Tab [6,1] [2],
--     Tab [7,1] [2], Tab [8,1] [2], Tab [9,1] [2]]
-- ---------------------------------------------------------------------
 
siguientesTableros :: Tablero -> [Tablero]
siguientesTableros t 
    | tieneGanador t = [] 
    | otherwise      = map (pone t) (posicionesLibres t)
 
-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    construyeArbol :: Tablero -> Arbol Tablero
-- tal que (construyeArbol t) es el árbol de juego correspondiente al
-- tablero t. Por ejemplo,
--    ghci> construyeArbol (Tab [7,1,6,2] [5,4,3])
--     Tab [7,1,6,2] [5,4,3]
--       Tab [7,1,6,2] [8,5,4,3]
--         Tab [9,7,1,6,2] [8,5,4,3]
--       Tab [7,1,6,2] [9,5,4,3]
--         Tab [8,7,1,6,2] [9,5,4,3]
-- ---------------------------------------------------------------------
 
construyeArbol :: Tablero -> Arbol Tablero
construyeArbol t = 
    Nodo t (map construyeArbol (siguientesTableros t)) 
 
-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir el tipo Valor para representa el valor de los
-- tableros. Los valores son números enteros.
-- ---------------------------------------------------------------------
 
type Valor = Int
 
-- ---------------------------------------------------------------------
-- Ejercicio 18. Un tablero valorado es un par de la forma (v,t) donde t
-- es un tablero y v es el valor del tablero.
-- 
-- Definir la función
--    valores :: [Arbol (Valor,Tablero)] -> [Valor]
-- tal que (valores vts) es la lista de valores de la lista de árboles de
-- tableros valorados vts. Por ejemplo, 
--    ghci> valores [Nodo (2,Tab[][])[], Nodo (5,Tab[3][])[]] 
--    [2,5]
-- ---------------------------------------------------------------------
 
valores :: [Arbol (Valor,Tablero)] -> [Valor]
valores vts = [v | Nodo (v,_) _ <- vts]
 
-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    maximiza :: Arbol Tablero -> Arbol (Valor,Tablero)
-- tal que (maximiza at) es el árbol de tableros máximamente valorados
-- correspondiente al árbol de tableros at mediante el algoritmo
-- minimax; es decir, 
-- * si at es un árbol con raíz t y sin subárboles, entonces
--   * si t tiene ganador, devuelve (Nodo (-1,t) [])
--   * en caso contrario,  devuelve (Nodo ( 0,t) [])
-- * si at es un árbol con raíz t y subárboles ts, entonces minimiza los
--   árboles de ts (construyendo una lista vts de árboles valorados) y
--   devuelve (Nodo (m,t) vts), donde m es el máximo de los valores de
--   vts.    
-- ---------------------------------------------------------------------
 
maximiza :: Arbol Tablero -> Arbol (Valor,Tablero)
maximiza (Nodo t []) | tieneGanador t = Nodo (-1,t) []
                     | otherwise      = Nodo (0,t) []                                        
maximiza (Nodo t ts) = Nodo (maximum (valores vts),t) vts
    where vts = map minimiza ts
 
-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    minimiza :: Arbol Tablero -> Arbol (Valor,Tablero)
-- tal que (minimiza at) es el árbol de tableros mínimamente valorados
-- correspondiente al árbol de tableros at mediante el algoritmo
-- minimax; es decir, 
-- * si at es un árbol con raíz t y sin subárboles, entonces
--   * si t tiene ganador, devuelve (Nodo (1,t) [])
--   * en caso contrario,  devuelve (Nodo (0,t) [])
-- * si at es un árbol con raíz t y subárboles ts, entonces maximiza los
--   árboles de ts (construyendo una lista vts de árboles valorados) y
--   devuelve (Nodo (m,t) vts), donde m es el mínimo de los valores de
--   vts.    
-- ---------------------------------------------------------------------
 
minimiza :: Arbol Tablero -> Arbol (Valor,Tablero)
minimiza (Nodo t []) | tieneGanador t = Nodo (1,t) []
                     | otherwise      = Nodo (0,t) []
minimiza (Nodo t ts) = Nodo (minimum (valores vts),t) vts
    where vts = map maximiza ts
 
-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    poda :: Int -> Arbol a -> Arbol a
-- tal que (poda n a) es el árbol obtenido podando el árbol a a
-- partir de la profundidad n. Por ejemplo,
--    ghci> Nodo 4 [Nodo 3 [], Nodo 7 [Nodo 9 [Nodo 2 []]]]
--    4
--      3
--      7
--        9
--          2
--    
--    ghci> poda 2 (Nodo 4 [Nodo 3 [], Nodo 7 [Nodo 9 [Nodo 2 []]]])
--    4
--      3
--      7
--        9
--    
--    ghci> poda 1 (Nodo 4 [Nodo 3 [], Nodo 7 [Nodo 9 [Nodo 2 []]]])
--    4
--      3
--      7
-- ---------------------------------------------------------------------
 
poda :: Int -> Arbol a -> Arbol a
poda n (Nodo x as) | n == 0    = Nodo x []
                   | otherwise = Nodo x (map (poda (n-1)) as)
 
-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    selecciona :: Arbol (Valor,Tablero) -> Tablero
-- tal que (selecciona avts) es el tablero del primer hijo de la raíz del
-- árbol de tableros valorados avts cuyo valor es igual que la raíz.
-- ---------------------------------------------------------------------
 
selecciona :: Arbol (Valor,Tablero) -> Tablero
selecciona (Nodo (v,_) ts) = 
    head [t | Nodo (v',t) _ <- ts, v'==v]
 
-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    mejorMovimiento :: Tablero -> Tablero
-- tal que (mejorMovimiento t) es el tablero correspondiente al mejor
-- movimiento a partir del tablero t. Por ejemplo,
--    mejorMovimiento (Tab [3] [])     ==  Tab [3] [5]
--    mejorMovimiento (Tab [4,3] [5])  ==  Tab [4,3] [1,5]
-- ---------------------------------------------------------------------
 
mejorMovimiento :: Tablero -> Tablero
mejorMovimiento = 
    selecciona . maximiza . poda profundidadDeBusqueda . construyeArbol
 
-- ---------------------------------------------------------------------
-- § Dibujo del tablero                                               --
-- ---------------------------------------------------------------------
 
-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    muestraPosicion :: Tablero -> Posicion -> String
-- tal que (muestraPosicion t p) es el contenido de la posición p del
-- tablero t; es decir, X si p está en la lista de las xs; O si p está
-- en la lista de las os y la cadena de p, en otro caso. Por ejemplo,
--    ghci> muestraPosicion (Tab [1] [3]) 1
--    "X"
--    ghci> muestraPosicion (Tab [1] [3]) 3
--    "O"
--    ghci> muestraPosicion (Tab [1] [3]) 2
--    "2"
-- ---------------------------------------------------------------------
 
muestraPosicion :: Tablero -> Posicion -> String
muestraPosicion (Tab xs os) p 
    | p `elem` xs = "X"
    | p `elem` os = "O"
    | otherwise   = show p
 
-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    muestraLinea :: Tablero -> [Posicion] -> String
-- tal que (muestraLinea t ps) es la cadena correspondiente al contenido
-- de las posiciones ps en el tablero t separadas por la barra
-- vertical. Por ejemplo,
--    ghci> muestraLinea (Tab [7,1,6,2] [8,4,3]) [4..6]
--    "O|5|X"
-- ---------------------------------------------------------------------
 
muestraLinea :: Tablero -> [Posicion] -> String
muestraLinea t = 
    concat . intersperse "|" . map (muestraPosicion t)
 
-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    muestraTablero :: Tablero -> String
-- tal que (muestraTablero t) es la cadena correspondiente al tablero
-- t. Por ejemplo, 
--    ghci> muestraTablero (Tab [7,1,6,2] [8,4,3])
--    "X|X|O\n-+-+-\nO|5|X\n-+-+-\nX|O|9"
--    ghci> putStrLn (muestraTablero (Tab [7,1,6,2] [8,4,3]))
--    X|X|O
--    -+-+-
--    O|5|X
--    -+-+-
--    X|O|9
-- ---------------------------------------------------------------------

muestraTablero :: Tablero -> String
muestraTablero t = 
    muestraLinea t [1..3] ++ "\n-+-+-\n" ++
    muestraLinea t [4..6] ++ "\n-+-+-\n" ++
    muestraLinea t [7..9]
 
-- ---------------------------------------------------------------------
-- § Control del juego                                                --
-- ---------------------------------------------------------------------
 
-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    main :: IO ()
-- que controle el juego siguiendo los siguientes pasos:
--    1. Activa la escritura inmediata en la pantalla.
--    2. Escribe el nombre del juego.
--    3. Escribe el tablero inicial.
--    4. Pregunta al humano si desea comenzar el juego.
--    5. Para y lee la respuesta.
--    6. Comprueba si la respuesta es afirmativa.
--    7. En el caso que la respuesta sea afirmativa, realiza un
--       movimiento del jugador humano.  
--    8. En el caso que la respuesta sea negativa, realiza un movimiento
--       de la computadora. 
-- ---------------------------------------------------------------------
 
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering           -- 1
  putStrLn "Tres en raya"                    -- 2
  putStrLn (muestraTablero tableroInicial)   -- 3
  putStr "Comienza el juego? (s/n) "         -- 4
  l <- getLine                               -- 5
  if head l `elem` "sS"                      -- 6
     then humano tableroInicial              -- 7
     else computadora tableroInicial         -- 8
 
-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    humano :: Tablero -> IO ()
-- tal que (humano t) realiza el movimiento del jugador humano a partir
-- del tablero t. Consta de los siguientes pasos:
--    1. Pregunta la posición en donde desea colocar la ficha.
--    2. Lee la posición en donde desea colocar la ficha.
--    3. Calcula el tablero t' correspondiente a colocar la ficha en la
--       posición elegida.
--    4. Muestra el tablero t'.
--    5. Decide si t' tiene ganador.
--       5.a. En caso afirmativo, escribe que el jugador humano ha ganado.
--       5.b. En caso negativo, decide si el tablero está completo
--            5.b.1. En caso afirmativo, escribe que hay empate.
--            5.b.2. En caso negativo, pasa el turno a la computadora
--                   con tablero t'. 
-- 
-- Nota: No se comprueba la corrección de la posición elegida (es decir, 
-- si es un número entre 1 y 9 y no hay ficha en esa posición).
-- ---------------------------------------------------------------------
 
humano :: Tablero -> IO ()
humano t = do 
  putStr "\nIndica el lugar donde colocar la ficha: " -- 1
  l <- getLine                                        -- 2 
  let t' = pone t (read l :: Posicion)                -- 3
  putStrLn (muestraTablero t')                        -- 4
  if tieneGanador t'                                  -- 5
     then putStrLn "Has ganado."                      -- 5.a
     else if completo t'                              -- 5.b
             then putStrLn "Empate."                  -- 5.b.1
             else computadora t'                      -- 5.b.2
 
-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--    computadora :: Tablero -> IO ()
-- tal que (computadora t) realiza el movimiento de la computadora a
-- partir del tablero t. Consta de los siguientes pasos:
--    1. Escribe la jugada de la computadora 
--    2. Calcula el tablero t' correspondiente al mejor movimiento en
--       t. 
--    3. Escribe t'.
--    4. Decide si t' tiene ganador.
--       4.a. En caso afirmativo, escribe que la computadora ha ganado.
--       4.b. En caso negativo, decide si el tablero está completo.
--            4.b.1. En caso afirmativo, escribe que hay empate.
--            4.b.2. En caso negativo, pasa el turno al humano con
--                   tablero t'. 
-- ---------------------------------------------------------------------
 
computadora :: Tablero -> IO ()
computadora t = do
  putStrLn "\nMi jugada:"            -- 1
  let t' = mejorMovimiento t         -- 2
  putStrLn (muestraTablero t')       -- 3
  if tieneGanador t'                 -- 4
     then putStrLn "He ganado."      -- 4.a
     else if completo t'             -- 4.b
             then putStrLn "Empate." -- 4.b.1
             else humano t'          -- 4.b.2