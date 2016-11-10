module Nido
    ( Nido.run
    ) where

import Control.Concurrent

import Concumon

run :: Int -> IO ()
run tiempoMovConcumon = do
	putStrLn ("Corriendo Nido")
	putStrLn "Creando concumones"
	idConcumon <- forkIO(Concumon.run tiempoMovConcumon)

	threadDelay	5000000
	putStrLn "Finalizando Nido"