module Nido
    ( Nido.run
    ) where

import Concumon

run :: Int -> IO ()
run tiempoMovConcumon = do
	putStrLn ("Corriendo Nido")
	putStrLn "Creando concumones"
	Concumon.run tiempoMovConcumon