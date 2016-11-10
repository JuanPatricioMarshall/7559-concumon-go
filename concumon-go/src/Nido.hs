module Nido
    ( Nido.run
    ) where

import Control.Concurrent
import Control.Monad

import Concumon

run :: Int -> Chan String -> IO ()
run tiempoMovConcumon mapaChan = do
	putStrLn ("Corriendo Nido")
	-- TODO: Ver si esta bien con forever, o usar otra cosa
	forever $ do
		putStrLn "Creando concumon"
		idConcumon <- forkIO(Concumon.run tiempoMovConcumon mapaChan)
		threadDelay	5000000
	putStrLn "Finalizando Nido"