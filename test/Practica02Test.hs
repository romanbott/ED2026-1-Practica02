module Main (main) where

import Test.Hspec
import Practica02
import Test.Hspec.Runner
import Data.Char (toUpper)


main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do

    let bin8 =  [I,O,O,O]
    let bin19 = [I,O,O,I,I]
    let bin26 = [I,I,O,I,O]
    let bin54 = [I,I,O,I,I,O]
    
    describe "Tests toDecimal" $ do
        it "8 en binario" $ do
            toDecimal bin8 `shouldBe` 8
        
        it "19 en binario" $ do
            toDecimal bin19 `shouldBe` 19

        it "26 en binario" $ do
            toDecimal bin26 `shouldBe` 26

        it "54 en binario" $ do
            toDecimal bin54 `shouldBe` 54

    describe "Tests toBin" $ do
        it "8 en decimal" $ do
            toBin 8 `shouldBe` bin8
        
        it "19 en decimal" $ do
            toBin 19 `shouldBe` bin19

        it "26 en decimal" $ do
            toBin 26 `shouldBe` bin26

        it "54 en decimal" $ do
            toBin 54 `shouldBe` bin54
        
    describe "Tests suma" $ do
        it "8 mas 19" $ do
            suma bin8 bin19 `shouldBe` [I,I,O,I,I]
        
        it "54 mas 26" $ do
            suma bin54 bin26 `shouldBe` [I,O,I,O,O,O,O]

        it "19 mas 26" $ do
            suma bin19 bin26 `shouldBe` [I,O,I,I,O,I]

    describe "Tests palindromo" $ do
        it "Verdadero" $ do
            palindromo "anitalavalatina" `shouldBe` True
        
        it "Falso" $ do
            palindromo [1,2,3,4,5,6,7,8,9,10] `shouldBe` False
        
    describe "Test diferenciaSimetrica" $ do
        it "Listas disjuntas" $ do
            diferenciaSimetrica [1,2] [3,4,5] `shouldMatchList` [1,2,3,4,5]
        
        it "Lista iguales" $ do
            diferenciaSimetrica ["x", "y", "z"] ["x", "y", "z"] `shouldMatchList` []

        it "Listas parcialmente intersectadas" $ do
            diferenciaSimetrica "abcdefg" "efghijk" `shouldMatchList` "abcdhijk"

    describe "Test conjuntoPotencia" $ do
        it "Lista enteros" $ do
            conjuntoPotencia [1,2] `shouldMatchList` [[1,2], [1], [2], []]
        
        it "Lista cadenas" $ do
            conjuntoPotencia ["x", "y", "z"] `shouldMatchList` [["x","y","z"], ["x","y"], ["x","z"], ["x"], ["y","z"], ["y"], ["z"], []]

        it "Cadena" $ do
            conjuntoPotencia "abc" `shouldMatchList` ["abc", "ab", "ac", "a", "bc", "b", "c", ""]

    describe "Tests longitud" $ do
        it "Longitud 6" $ do
            longitud [(1,2),(3,4),(5,6)] `shouldBe` 6 
        
        it "Longitud 10" $ do
            longitud [(1,2),(6,5),(8,19),(32,90),(15,72)] `shouldBe` 10 

    describe "Tests myMap" $ do
        it "Pares de enteros" $ do
            myMap (*2) (*3) [(1,2), (3,4), (5,6)] `shouldMatchList` [(2,6),(6,12),(10,18)]
        
        it "Par de cadenas con enteros" $ do
            myMap (map toUpper) (*2) [("hola",1),("adios",2)] `shouldMatchList` [("HOLA",2),("ADIOS",4)]
    
    describe "Tests sumaPares" $ do
        it "Resultado (6,60)" $ do
            sumaPares [(1,10),(2,20),(3,30)] `shouldBe` (6,60)
        
        it "Resultado (0.6,1.6)" $ do
            sumaPares [(0.1,0.9),(0.2,0.3),(0.3,0.4)] `shouldBe` (0.6,1.6)
    
    describe "Tests myFilter" $ do
        it "Lista de pares booleanos" $ do
            myFilter (\(x,y) -> x && y) [(True,True),(True,False),(False,True)] `shouldMatchList` [(True,True)]
        
        it "Lista de pares string" $ do
            myFilter (\(x,y) -> length x < length y) [("hola","adios"),("si","noo"),("abc","d")] `shouldMatchList` [("hola","adios"),("si","noo")]

        it "Lista de pares numericos" $ do
            myFilter (\(x,y) -> x > y) [(1,2),(3,1),(5,5),(10,3)] `shouldMatchList` [(3,1),(10,3)]

    