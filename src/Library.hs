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

-- chico :: Chico
-- chico = Chico "bobi" 10 ["Ser un supermodelo noruego"] [serMayor]   

envioX :: Envio
-- envioX = UnEnvio origen         Destino       Peso PrecioBase [Categorias]
envioX = UnEnvio ("CABA", "ARG") ("TUC","ARG") 10 1000 ["Consola"] [iva]

-- precioBase :: Envio -> Envio
-- precioBase envio = precioBase envio

cargoCategorico ::  Categoria -> Porcentaje -> Envio -> Envio
cargoCategorico  categoriaX porcentajeAComputar envio
        | elem categoriaX $categorias envio = envio { precioBase = calcularPorcentaje porcentajeAComputar envio}
        | otherwise = envio

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

-- precioBruto :: Envio -> Number
-- precioBruto :: 

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

