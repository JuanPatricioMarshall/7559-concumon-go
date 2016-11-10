module Nido
    ( Nido.run
    ) where

import Control.Concurrent
import Control.Monad

import Concumon

run :: Int -> IO ()
run tiempoMovConcumon = do
	putStrLn ("Corriendo Nido")
	-- TODO: Ver si esta bien con forever, o usar otra cosa
	forever $ do
		putStrLn "Creando concumon"
		idConcumon <- forkIO(Concumon.run tiempoMovConcumon)
		threadDelay	5000000
	putStrLn "Finalizando Nido"