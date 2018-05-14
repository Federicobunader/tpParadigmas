module MicroEntrega1 where
type Posicion = Int
data Microcontrolador = Microcontrolador {memoriaDeDatos :: [Posicion], acumA :: Int, acumB :: Int, contadorPrograma :: Int, ultimoErrorProducido :: String} deriving (Show, Eq)
xt8088 = Microcontrolador {memoriaDeDatos = replicate 1024 0, acumA = 2, acumB = 4, contadorPrograma = 0, ultimoErrorProducido = ""}
fp20 = Microcontrolador {memoriaDeDatos = replicate 1024 0, acumA = 7, acumB = 24, contadorPrograma = 0, ultimoErrorProducido = ""}
at8086 = Microcontrolador {memoriaDeDatos = [1..20], acumA = 0, acumB = 0, contadorPrograma = 0, ultimoErrorProducido = ""}
nop :: Microcontrolador -> Microcontrolador
nop microcontrolador = aumentarContador microcontrolador
lodv :: Int -> Microcontrolador -> Microcontrolador
lodv val microcontrolador = aumentarContador microcontrolador {acumA = val}
swap :: Microcontrolador -> Microcontrolador
swap microcontrolador = aumentarContador microcontrolador {acumA = acumB microcontrolador, acumB = acumA microcontrolador}
swap2 :: Microcontrolador -> Microcontrolador
swap2 (Microcontrolador memoriaDeDatos acumA acumB contadorPrograma ultimoErrorProducido) = aumentarContador (Microcontrolador memoriaDeDatos acumB acumA contadorPrograma ultimoErrorProducido)
add :: Microcontrolador -> Microcontrolador
add microcontrolador = aumentarContador microcontrolador {acumA = acumA microcontrolador + acumB microcontrolador, acumB = 0}
divide :: Microcontrolador-> Microcontrolador
divide microcontrolador | acumB microcontrolador == 0 = aumentarContador microcontrolador {ultimoErrorProducido = "DIVISION BY ZERO"}
                        | otherwise = aumentarContador microcontrolador {acumA = div (acumA microcontrolador) (acumB microcontrolador), acumB = 0}
str :: Posicion -> Int -> Microcontrolador -> Microcontrolador
str addr val microcontrolador = aumentarContador microcontrolador { memoriaDeDatos = insertarEnMemoria addr val (memoriaDeDatos microcontrolador)}
insertarEnMemoria :: Posicion -> Int -> [Posicion] -> [Posicion]
insertarEnMemoria addr val lista = (take (addr-1) lista) ++ [val] ++ (drop addr lista)
lod :: Posicion -> Microcontrolador -> Microcontrolador
lod addr microcontrolador = aumentarContador microcontrolador { acumA = (memoriaDeDatos microcontrolador) !! addr}
aumentarContador :: Microcontrolador -> Microcontrolador
aumentarContador microcontrolador = microcontrolador {contadorPrograma = contadorPrograma microcontrolador + 1}









