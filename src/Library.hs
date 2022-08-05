{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Library where
import PdePreludat
import GHC.Num (Num)



type Ciudad = String
type Pais = String
type Porcentaje = Number
type Peso = Number
type Precio = Number
type Categoria = String

data Envio = UnEnvio {
    origen :: (Ciudad,Pais),
    destino :: (Ciudad,Pais),
    peso :: Peso,
    precioBase :: Precio,
    categorias :: [Categoria],
    impuestos :: [Impuesto]
    } deriving (Show,Eq)

envioARG :: Envio
-- envioX = UnEnvio origen         Destino       Peso PrecioBase [Categorias]
envioARG = UnEnvio ("CABA", "ARG") ("TUC","ARG") 10 1000 ["Consola"] [iva]

-- precioBase :: Envio -> Envio
-- precioBase envio = precioBase envio

cargoCategorico ::  Categoria -> Porcentaje -> Envio -> Envio
cargoCategorico  categoriaX porcentajeAComputar envio
        | tieneLaCategoriaX categoriaX envio = envio { precioBase = calcularPorcentaje porcentajeAComputar envio}
        | otherwise = envio

tieneLaCategoriaX :: Categoria -> Envio -> Bool
tieneLaCategoriaX categoriaX   = elem categoriaX . categorias

calcularPorcentaje :: Porcentaje -> Envio -> Number
calcularPorcentaje  porcentajeX envio = (porcentajeX * precioBase envio) / 100 --Porcentaje del 1 al 100

cargoPorSobrePeso :: Peso -> Envio -> Envio
cargoPorSobrePeso pesoLimite envio
        | peso envio < pesoLimite = envio
        | otherwise = agregarCosto  pesoLimite envio

agregarCosto :: Peso -> Envio -> Envio
agregarCosto  pesoLimite envio  = envio { precioBase = precioBase envio + 80 * (peso envio - pesoLimite)}

-- cargoArbitrario adiciono 50 pesos al envio  
-- cargoArbitrario :: Envio -> Envio  
cargoArbitrario :: Envio -> Envio
cargoArbitrario envio = envio {precioBase = precioBase envio + 50 }


-- tipo de un cargo
type Cargo =  Envio -> Envio

type Impuesto = Envio -> Number


-- IVA : 20% del precio
-- iva :: Envio -> Number
iva :: Impuesto
iva = calcularPorcentaje 20

-- con guardas 

multicategoria :: Impuesto
multicategoria envioX
    | tieneMasDeTresCategorias envioX = calcularPorcentaje 1 envioX --
    | otherwise = precioBase envioX --mantengo el precio 

tieneMasDeTresCategorias :: Envio -> Bool
tieneMasDeTresCategorias = (>3).length.categorias


--aduanero
-- funciones auxiliares

-- paisOrigen :: Emvio


paisOrigen :: Envio -> Pais
paisOrigen = snd.origen

paisDestino :: Envio -> Pais
paisDestino = snd.destino

aduanero :: Impuesto
aduanero envioX
    | elEnvioEsInternacional envioX = calcularPorcentaje 3 envioX
    | otherwise = precioBase envioX

elEnvioEsInternacional :: Envio -> Bool
elEnvioEsInternacional envioX = paisOrigen envioX /= paisDestino envioX

impuestoExtranio :: Impuesto
impuestoExtranio envioX
    | tienePrecioPar envioX = calcularPorcentaje 10 envioX
    | otherwise = precioBase envioX

tienePrecioPar :: Envio -> Bool
tienePrecioPar = even.precioBase

-- Tengo repeticion de logica

-- 2a. Un cargo categórico de “tecnología” de 18%. 

cargoTecnologico :: Envio
cargoTecnologico = cargoCategorico "tecnologia" 18 envioARG

-- 2b. Envío con origen en Buenos Aires, Argentina y con destino Utrecht, Países Bajos, 
-- de 2kg de peso, precio base de $220, con las categorías de música y tecnología, 
-- sin impuestos.


envioPaisesBajos :: Envio
-- envioX = UnEnvio origen   Destino    Peso PrecioBase [Categorias]
envioPaisesBajos = UnEnvio ("BSAS", "ARG") ("ULT","HOL") 2 220 ["musica","tecnologia"] [ ]


-- 2c.Envío con origen California, Estados Unidos y con destino Miami, Estado Unidos, 
-- de 5kg de peso, precio base $1500, con categoría de libros, y con IVA e impuesto 
-- extraño.

envioEEUU :: Envio
envioEEUU = UnEnvio ("BSAS", "ARG") ("ULT","HOL") 5 1500 ["libros"] [ iva, impuestoExtranio ]

-- 3.a Saber si el precio base de un envío cuesta más que un valor determinado N.

cuestaMasQuePrecio_N ::  Precio -> Envio -> Bool
cuestaMasQuePrecio_N  precioN  =  (> precioN).precioBase

--3.b Conocer si un  envío  es  barato. Decimos que  es barato si vale $1300 o menos . (precio base).

elEnvioEsBarato :: Envio -> Bool
elEnvioEsBarato  = (< 1300).precioBase

-- 4a. Saber si un envío se dirige a un país determinado

seDirigeA :: Pais -> Envio -> Bool
seDirigeA destino = (== destino).paisDestino

-- 4b. Dado un envío, determinar si  es local  o es internacional. Es local  cuando los 
-- países de origen y de destino son iguales.

-- elEnvioEsInternacional :: Envio -> Bool
-- elEnvioEsInternacional envioX = paisOrigen envioX /= paisDestino envioX

elEnvioEsLocal :: Envio -> Bool
elEnvioEsLocal envioX = paisOrigen envioX == paisDestino envioX

-- 5. A  partir  de  un  conjunto  de  envíos,  obtener  aquellos  que  tienen  ciertas  categorías. 
-- Nota: No se puede usar expresiones lambda, definiciones locales ni funciones auxiliares.

-- data Envio = UnEnvio {
--     origen :: (Ciudad,Pais),
--     destino :: (Ciudad,Pais),
--     peso :: Peso,
--     precioBase :: Precio,
--     categorias :: [Categoria],
--     impuestos :: [Impuesto]
--     } deriving (Show,Eq)

-- filtrarPorCategorias :: [Categoria] -> [Envio] -> [Envio]

-- tieneLaCategoriaX :: Categoria -> Envio -> Bool

-- buscarCategoria :: Categoria -> [Envio] -> [Envio]
-- buscarCategoria categoriaX = takeWhile (tieneLaCategoriaX categoriaX)




conjuntoDeEnvios :: [Envio]
conjuntoDeEnvios = [envioARG,envioEEUU,envioPaisesBajos]

categoriasBusqueda :: [Categoria]
categoriasBusqueda = ["musica","tecnologia","ropa"]




 