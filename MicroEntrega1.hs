module MicroEntrega1 where

type Posicion = Int
data Microcontrolador = Microcontrolador {memoriaDeDatos :: [Posicion], acumA :: Int, acumB :: Int, contadorPrograma :: Int, ultimoErrorProducido :: String} deriving (Show, Eq)

xt8088 = Microcontrolador {memoriaDeDatos = replicate 1024 0, acumA = 0, acumB = 0, contadorPrograma = 0, ultimoErrorProducido = ""}
fp20 = Microcontrolador {memoriaDeDatos = replicate 1024 0, acumA = 7, acumB = 24, contadorPrograma = 0, ultimoErrorProducido = ""}
at8086 = Microcontrolador {memoriaDeDatos = [1..20], acumA = 0, acumB = 0, contadorPrograma = 0, ultimoErrorProducido = ""}

nop :: Microcontrolador -> Microcontrolador
nop microcontrolador = microcontrolador {contadorPrograma = contadorPrograma microcontrolador + 1}

lodv :: Int -> Microcontrolador -> Microcontrolador
lodv val microcontrolador = microcontrolador {acumA = val, contadorPrograma = contadorPrograma microcontrolador + 1}

swap :: Microcontrolador -> Microcontrolador
swap microcontrolador = microcontrolador {acumA = acumB microcontrolador, acumB = acumA microcontrolador, contadorPrograma = contadorPrograma microcontrolador + 1}

add :: Microcontrolador -> Microcontrolador
add microcontrolador = microcontrolador {acumA = acumA microcontrolador + acumB microcontrolador, acumB = 0, contadorPrograma = contadorPrograma microcontrolador + 1}

divide :: Microcontrolador-> Microcontrolador
divide microcontrolador | acumB microcontrolador == 0 = microcontrolador{ultimoErrorProducido = "DIVISION BY ZERO", contadorPrograma = contadorPrograma microcontrolador + 1}
                        | otherwise = microcontrolador {acumA = acumA microcontrolador `div` acumB microcontrolador, acumB = 0, contadorPrograma = contadorPrograma microcontrolador + 1}

str :: Posicion -> Int -> Microcontrolador -> Microcontrolador
str addr val microcontrolador = microcontrolador { memoriaDeDatos = insertarEnMemoria addr val (memoriaDeDatos microcontrolador), contadorPrograma = contadorPrograma microcontrolador + 1}

insertarEnMemoria :: Posicion -> Int -> [Posicion] -> [Posicion]
insertarEnMemoria addr val lista = (take (addr-1) lista) ++ [val] ++ (drop addr lista)

lod :: Posicion -> Microcontrolador -> Microcontrolador
lod addr microcontrolador = microcontrolador { acumA = (memoriaDeDatos microcontrolador) !! addr, contadorPrograma = contadorPrograma microcontrolador + 1}

-- FUNCIONES PEDIDAS PARA LA ENTREGA:

programCounter :: Microcontrolador -> Int
programCounter microcontrolador = contadorPrograma microcontrolador 

acumuladorA :: Microcontrolador -> Int
acumuladorA microcontrolador =  acumA microcontrolador

acumuladorB :: Microcontrolador -> Int
acumuladorB microcontrolador = acumB microcontrolador

memoria :: Microcontrolador -> Int
memoria microcontrolador = length (memoriaDeDatos microcontrolador)

mensajeError :: Microcontrolador -> String
mensajeError microcontrolador = ultimoErrorProducido microcontrolador





