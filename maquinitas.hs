type Nombre = String
type Dinero = Float
type Suerte = Float
type Valor = Float

data Amuleto = Amuleto {
    nombreAmuleto :: Nombre,
    suerteAmuleto :: Suerte
} deriving (Show, Eq)

data Persona = Persona {
    nombre :: Nombre,
    dinero :: Dinero,
    suerte :: Suerte,
    factorSuerte :: [Amuleto]
} deriving (Show, Eq)

paciencia :: Amuleto
paciencia = Amuleto {
    nombreAmuleto = "Paciencia",
    suerteAmuleto = 0
}

inteligencia :: Amuleto
inteligencia = Amuleto {
    nombreAmuleto = "Inteligencia",
    suerteAmuleto = 3.23
}

soulStone :: Amuleto
soulStone = Amuleto {
    nombreAmuleto = "Soul Stone",
    suerteAmuleto = 5.03
}

mindStone :: Amuleto
mindStone = Amuleto {
    nombreAmuleto = "Mind Stone",
    suerteAmuleto = 10.66
}

tonyStark :: Persona
tonyStark = Persona {
    nombre = "Tony Stark",
    dinero = 158900.99,
    suerte = 10.66,
    factorSuerte = [inteligencia, soulStone]
}

bruceBanner :: Persona
bruceBanner = Persona {
    nombre = "Bruce Banner",
    dinero = 190.22,
    suerte = 10.5,
    factorSuerte = [paciencia, mindStone, inteligencia]
}

vision :: Persona
vision = Persona {
    nombre = "Vision",
    dinero = 1000,
    suerte = 5.1,
    factorSuerte = [mindStone, inteligencia]
}

-- 1)
suerteTotalDe :: Persona -> Suerte
suerteTotalDe persona
  | tieneAmuletos persona = (sum . aplicarFactorSuerte) persona
  | otherwise = suerte persona

tieneAmuletos :: Criterio
tieneAmuletos = (>0) . length . factorSuerte 

aplicarFactorSuerte :: Persona -> [Suerte]
aplicarFactorSuerte persona = map (suertePersonaAmuleto persona) (filtroAmuletoSuerte0 persona)

suertePersonaAmuleto :: Persona -> Amuleto -> Suerte
suertePersonaAmuleto persona = (suerte persona *) . suerteAmuleto

filtroAmuletoSuerte0 :: Persona -> [Amuleto]
filtroAmuletoSuerte0 persona = filter amuletoEsCero (factorSuerte persona)

amuletoEsCero :: Amuleto -> Bool
amuletoEsCero = (/= 0) . suerteAmuleto

-- 2)
data JuegoDeApuesta = JuegoDeApuesta {
    nombreJuego :: Nombre,
    premio :: Premio,
    criterio :: Criterio
}

ruleta :: JuegoDeApuesta
ruleta = JuegoDeApuesta {
    nombreJuego = "Ruleta",
    premio = premioRuleta,
    criterio = criterioRuleta
}

type Premio = Dinero -> Dinero
type Criterio = Persona -> Bool

premioRuleta :: Premio
premioRuleta = (* 37)

criterioRuleta :: Criterio
criterioRuleta = suertePersonaMayorA 80

maquinita :: Dinero -> JuegoDeApuesta
maquinita n = JuegoDeApuesta {
    nombreJuego = "Maquinita",
    premio = premioMaquinita n,
    criterio = criterioMaquinita
}

premioMaquinita :: Dinero -> Premio
premioMaquinita jackpot = (jackpot +)

criterioMaquinita :: Criterio
criterioMaquinita persona = suertePersonaMayorA 95 persona && personaEsPaciente persona

personaEsPaciente :: Criterio
personaEsPaciente persona = elem paciencia (factorSuerte persona)

suertePersonaMayorA :: Float -> Persona -> Bool
suertePersonaMayorA numero = (> numero) . suerteTotalDe

-- 3)
ganaEnJuego :: JuegoDeApuesta -> Persona -> Bool
ganaEnJuego juego = criterio juego

-- 4)
type Casino = [JuegoDeApuesta]

casinoRoyale :: Casino
casinoRoyale = [ruleta, maquinita 9]

dineroTotalObtenido :: Persona -> Dinero -> Casino -> Dinero 
dineroTotalObtenido persona apuesta casino = sum $ map (gananciaPorJuego persona apuesta) casino

gananciaPorJuego :: Persona -> Dinero -> JuegoDeApuesta -> Dinero
gananciaPorJuego persona apuesta juego
    | ganaEnJuego juego persona = premio juego apuesta
    | otherwise = apuesta

-- 5)
type Grupo = [Persona]

avengers :: Grupo
avengers = [tonyStark, bruceBanner, vision]

nombresPerdedores :: Grupo -> Casino -> [Nombre]
nombresPerdedores grupo casino = map nombre (perdedores grupo casino)

perdedores :: Grupo -> Casino -> Grupo
perdedores grupo casino = filter (flip juegosPerdidos casino) grupo

juegosPerdidos :: Persona -> Casino -> Bool
juegosPerdidos persona casino = sum (contadorPerdidas persona casino) == length casino

contadorPerdidas :: Persona -> Casino -> [Int]
contadorPerdidas persona = map (pierdeEnJuego persona)

pierdeEnJuego :: Persona -> JuegoDeApuesta -> Int
pierdeEnJuego persona juego
    | not $ ganaEnJuego juego persona = 1
    | otherwise = 0

-- 6)
restarDinero :: Persona -> Dinero -> Persona
restarDinero persona apuesta = persona {dinero = dinero persona - apuesta}

sumarGanancia :: Persona -> Dinero -> Persona
sumarGanancia persona ganancia = persona {dinero = dinero persona + ganancia} 

apostarAlJuego :: Persona -> Dinero -> JuegoDeApuesta -> Persona
apostarAlJuego persona apuesta juego
    | (ganancia persona apuesta juego) == apuesta = restarDinero persona apuesta
    | otherwise = sumarGanancia (restarDinero persona apuesta) (ganancia persona apuesta juego)
    where ganancia persona apuesta juego = gananciaPorJuego persona apuesta juego