module Library where

import GHC.IO.Handle.Types (Handle__ (haBufferMode))
import PdePreludat

data Ingrediente = Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano | PanIntegral
  deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa
  { precioBase :: Number,
    ingredientes :: [Ingrediente]
  }
  deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {ingredientes = ingrediente : ingredientes hamburguesa}

agregarVarios :: [Ingrediente] -> Hamburguesa -> Hamburguesa
agregarVarios [] hamburguesa = hamburguesa
agregarVarios (x : xs) hamburguesa = agregarVarios xs (agregarIngrediente x hamburguesa)

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = precioBase hamburguesa + (sum . map precioIngrediente . ingredientes) hamburguesa

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
  | (elem Carne . ingredientes) hamburguesa = agregarIngrediente Carne hamburguesa
  | (elem Pollo . ingredientes) hamburguesa = agregarIngrediente Pollo hamburguesa
  | (elem PatiVegano . ingredientes) hamburguesa = agregarIngrediente PatiVegano hamburguesa
  | otherwise = hamburguesa

aplicarDescuento :: Number -> Number -> Number
aplicarDescuento porcentaje precio = precio * (1 - porcentaje)

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa {precioBase = aplicarDescuento porcentaje (precioBase hamburguesa)}

pdepBurger :: Hamburguesa
pdepBurger = descuento 0.2 . agregarVarios [Panceta, Cheddar] . agrandar . agrandar $ cuartoDeLibra

-- Parte 2 ---

dobleCuarto :: Hamburguesa
dobleCuarto = agregarVarios [Carne, Cheddar] cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 0.3 . agregarIngrediente Papas

-- PARTE 3 --

transformarIngrediente :: Ingrediente -> Ingrediente
transformarIngrediente Carne = PatiVegano
transformarIngrediente Pollo = PatiVegano
transformarIngrediente Cheddar = QuesoDeAlmendras
transformarIngrediente Panceta = BaconDeTofu
transformarIngrediente Pan = PanIntegral
transformarIngrediente otro = otro

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa =
  hamburguesa
    { ingredientes = map transformarIngrediente (ingredientes hamburguesa)
    }

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa =
  hamburguesa
    { ingredientes = map transformarIngrediente (ingredientes hamburguesa)
    }

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = hacerVeggie . cambiarPanDePati $ dobleCuarto
