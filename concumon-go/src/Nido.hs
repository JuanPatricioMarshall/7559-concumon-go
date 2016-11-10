module Nido
    ( Nido.run
    ) where

import Control.Concurrent
import Control.Monad

import Concumon

run :: Int -> Chan movimiento -> IO ()
run tiempoMovConcumon movesChan = do
	putStrLn ("Corriendo Nido")
	-- TODO: Ver si esta bien con forever, o usar otra cosa
	forever $ do
		putStrLn "Creando concumon"
		idConcumon <- forkIO(Concumon.run tiempoMovConcumon movesChan)
		threadDelay	5000000
	putStrLn "Finalizando Nido"