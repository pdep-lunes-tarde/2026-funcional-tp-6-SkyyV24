module Spec where

import Control.Exception (evaluate)
import GHC.RTS.Flags (ProfFlags (descrSelector))
import Library
import PdePreludat
import Test.Hspec

hamburguesaCarne = Hamburguesa 20 [Carne]

hamburguesaPollo = Hamburguesa 20 [Pollo]

hamburguesaPatiVegano = Hamburguesa 20 [PatiVegano]

correrTests :: IO ()
correrTests = hspec $ do
  describe "Parte 1" $ do
    describe "agrandar" $ do
      it "Dada una hamburguesa de carne se le agrega otra carne" $ do
        agrandar hamburguesaCarne `shouldBe` Hamburguesa 20 [Carne, Carne]
      it "Dada una hamburguesa de pollo se le agrega otro pollo" $ do
        agrandar hamburguesaPollo `shouldBe` Hamburguesa 20 [Pollo, Pollo]
      it "Dada una hamburguesa de pati vegano, se le agrega otro pati vegano" $ do
        agrandar hamburguesaPatiVegano `shouldBe` Hamburguesa 20 [PatiVegano, PatiVegano]

    describe "agregarIngrediente" $ do
      it "Dada una hamburguesa y un ingrediente, se le agrega a la hamburguesa dicho ingrediente" $ do
        agregarIngrediente Cheddar hamburguesaCarne `shouldBe` Hamburguesa 20 [Cheddar, Carne]

    describe "descuento" $ do
      it "Dado un descuento del 10%, el precio base de 20 debe bajar a 18" $ do
        descuento 0.1 hamburguesaCarne `shouldBe` Hamburguesa 18 [Carne]

    describe "pdepBurger" $ do
      it "La pdepBurger debe ser un cuartoDeLibra con Pacenta y Cheddar extra" $ do
        pdepBurger `shouldBe` Hamburguesa 16 [Cheddar, Panceta, Carne, Carne, Pan, Carne, Cheddar, Pan]
      it "La pdepBurguer debe tener un precio final de 110" $ do
        precioFinal pdepBurger `shouldBe` 110

    describe "dobleCuarto" $ do
      it "La dobleCuarto debe ser un cuartoDeLibra con Carne y Cheddar" $ do
        dobleCuarto `shouldBe` Hamburguesa 20 [Cheddar, Carne, Pan, Carne, Cheddar, Pan]
      it "El precio final de la dobleCuarto debe ser 84" $ do
        precioFinal dobleCuarto `shouldBe` 84

    describe "bigPdep" $ do
      it "La bigPdep es una dobleCuarto con Curry" $ do
        bigPdep `shouldBe` Hamburguesa 20 [Curry, Cheddar, Carne, Pan, Carne, Cheddar, Pan]
      it "El precio final de la bigPdep debe ser 89" $ do
        precioFinal bigPdep `shouldBe` 89

    describe "delDia" $ do
      it "Dada una hamburguesa, le agrega papas y le aplica un 30$ de descuento" $ do
        delDia hamburguesaCarne `shouldBe` Hamburguesa 14 [Papas, Carne]
      it "El precio final de una dobleCuarto delDia debe ser 88" $ do
        precioFinal (delDia (Hamburguesa 20 [Cheddar, Carne, Pan, Carne, Cheddar, Pan])) `shouldBe` 88

    describe "HacerVeggie" $ do
      it "Dada una hamburguesa, cambia su ingrediente base por PatiVegano, el Cheddar por QuesoAlmendras y la Panceta por BaconTofu" $ do
        let hamburguesaPrueba = Hamburguesa 20 [Carne, Cheddar, Panceta]
        hacerVeggie hamburguesaPrueba `shouldBe` Hamburguesa 20 [PatiVegano, QuesoDeAlmendras, BaconDeTofu]

    describe "cambiarPanDePati" $ do
      it "Dada una hamburguesa con pan, cambia el pan por panIntegral" $ do
        cambiarPanDePati (Hamburguesa 20 [Pan]) `shouldBe` Hamburguesa 20 [PanIntegral]

    describe "dobleCuartoVegano" $ do
      it "dobleCuartoVegano es como el dobleCuarto veggie pero tiene panIntegral en lugar de pan" $ do
        dobleCuartoVegano `shouldBe` Hamburguesa 20 [QuesoDeAlmendras, PatiVegano, PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral]