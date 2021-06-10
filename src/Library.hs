module Library where
import PdePreludat

data Gimnasta = UnGimnasta{
nombre :: String
, edad :: Number
, peso :: Number
, coeficienteDeTonificacion :: Number
} deriving (Show, Eq)

pancho = UnGimnasta "Francisco" 40 120 1
andres = UnGimnasta "Andy" 22 80 6

-- PUNTO 1

estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not . estaObeso) gimnasta && tonificacionMayorA5 gimnasta

estaObeso :: Gimnasta -> Bool
estaObeso = (>100) . peso

tonificacionMayorA5 :: Gimnasta -> Bool
tonificacionMayorA5 = (>5) . coeficienteDeTonificacion

-- PUNTO 2 

modificarPeso :: Number -> Gimnasta -> Gimnasta
modificarPeso numero gimnasta = gimnasta {peso = peso gimnasta + numero}

type Calorias = Number
quemarCalorias :: Calorias -> Gimnasta -> Gimnasta
quemarCalorias calorias gimnasta 
    | estaObeso gimnasta = modificarPeso (-unKiloCada150Calorias calorias) gimnasta
    | ((>30) . edad) gimnasta && calorias > 200 = modificarPeso (-1) gimnasta
    | otherwise = modificarPeso (-kilosPerdidosOtherwise calorias gimnasta) gimnasta

unKiloCada150Calorias :: Calorias -> Number
unKiloCada150Calorias = (/150) 

kilosPerdidosOtherwise :: Calorias -> Gimnasta -> Number
kilosPerdidosOtherwise calorias gimnasta = calorias/(edad gimnasta * peso gimnasta)

-- PUNTO 3 

type Ejercicio = Gimnasta -> Gimnasta

caminataEnCinta :: Minutos -> Ejercicio
caminataEnCinta minutos = quemarCalorias ((caloriasSegunVelPromedio 5 5) * minutos)

type Velocidad = Number
caloriasSegunVelPromedio :: Velocidad -> Velocidad -> Calorias
caloriasSegunVelPromedio velInicial  = (*1) . (velPromedio velInicial)

velPromedio :: Velocidad -> Velocidad -> Velocidad
velPromedio velInicial velFinal = (velInicial + velFinal) / 2
--------------------------------------------------------------------------------------------------------------------------------------------------
entrenamientoEnCinta :: Minutos -> Ejercicio
entrenamientoEnCinta minutos = quemarCalorias ((caloriasSegunVelPromedio 6 (velocidadFinal minutos)) * minutos)

velocidadFinal :: Minutos -> Velocidad 
velocidadFinal = (+6) . (/5)

--------------------------------------------------------------------------------------------------------------------------------------------------
type Minutos = Number
type Kilos = Number
pesas :: Kilos -> Minutos -> Ejercicio
pesas kilos minutos  
    | minutos > 10 = modificarTonificacion (decimaParte kilos) 
    | otherwise = id

modificarTonificacion :: Number -> Gimnasta -> Gimnasta
modificarTonificacion numero gimnasta = gimnasta {coeficienteDeTonificacion = coeficienteDeTonificacion gimnasta + numero}

decimaParte :: Number -> Number
decimaParte = (/10)
--------------------------------------------------------------------------------------------------------------------------------------------------
type Inclinacion = Number
colina :: Inclinacion -> Minutos -> Ejercicio 
colina inclinacion minutos = quemarCalorias (caloriasColina inclinacion minutos)

caloriasColina :: Inclinacion -> Minutos -> Calorias
caloriasColina inclinacion minutos = 2 * minutos * inclinacion
--------------------------------------------------------------------------------------------------------------------------------------------------
montania :: Inclinacion -> Minutos -> Ejercicio
montania inclinacion minutos = (modificarTonificacion 1) . (colina (inclinacionSegundaColina inclinacion) (minutosColina minutos)) . (colina inclinacion (minutosColina minutos))

minutosColina :: Minutos -> Minutos
minutosColina = (/2)

inclinacionSegundaColina :: Inclinacion -> Inclinacion
inclinacionSegundaColina = (+3)

-- PUNTO 4 

data Rutina = UnaRutina {
nombreRutina :: String
, duracion :: Number
, ejercicios :: [Ejercicio]
} deriving (Show, Eq)

potente = UnaRutina {
nombreRutina = "potente"
, duracion = 200
, ejercicios = [caminataEnCinta 40, entrenamientoEnCinta 40, pesas 50 40, colina 5 40, montania 5 40]
}
--------------------------------------------------------------------------------------------------------------------------------------------------
hacerUnEjercicio :: Ejercicio -> Gimnasta -> Gimnasta
hacerUnEjercicio ejercicio = ejercicio
--------------------------------------------------------------------------------------------------------------------------------------------------
hacerRutinaFold :: Rutina -> Gimnasta -> Gimnasta
hacerRutinaFold rutina gimnasta = foldl (flip hacerUnEjercicio) gimnasta (ejercicios rutina)
--------------------------------------------------------------------------------------------------------------------------------------------------
hacerRutinaRecursiva :: Rutina -> Gimnasta -> Gimnasta
hacerRutinaRecursiva (UnaRutina nombreRutina duracion []) gimnasta = gimnasta
hacerRutinaRecursiva (UnaRutina nombreRutina duracion (ejercicio:ejercicios)) gimnasta = hacerRutinaRecursiva (UnaRutina nombreRutina duracion ejercicios) (hacerUnEjercicio ejercicio gimnasta) 
--------------------------------------------------------------------------------------------------------------------------------------------------
type Nombre = String 
type Tonificacion = Number
resumenDeRutina :: Rutina -> Gimnasta -> (Nombre, Kilos, Tonificacion)
resumenDeRutina rutina gimnasta = (nombreRutina rutina, kilosPerdidos rutina gimnasta, tonificacionGanada rutina gimnasta)

kilosPerdidos :: Rutina -> Gimnasta -> Kilos
kilosPerdidos rutina gimnasta = peso gimnasta - (peso . (hacerRutinaFold rutina)) gimnasta 

tonificacionGanada :: Rutina -> Gimnasta -> Tonificacion
tonificacionGanada rutina gimnasta = (coeficienteDeTonificacion . (hacerRutinaFold rutina)) gimnasta - coeficienteDeTonificacion gimnasta

-- PUNTO 5

lograPonerSaludable :: Gimnasta -> [Rutina] -> [Rutina]
lograPonerSaludable gimnasta = filter (flip rutinaPoneSaludable gimnasta) 

rutinaPoneSaludable :: Rutina -> Gimnasta -> Bool
rutinaPoneSaludable rutina = estaSaludable . (hacerRutinaFold rutina)