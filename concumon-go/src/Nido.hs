module Nido
    ( Nido.run
    ) where

import Control.Concurrent
import Control.Monad

import Concumon

run :: QSem -> Int -> Chan String -> IO ()
run maxConcumonesSem tiempoMovConcumon mapaChan = do
	putStrLn ("Corriendo Nido")
	-- TODO: Ver si esta bien con forever, o usar otra cosa
	forever $ do
		waitQSem maxConcumonesSem
		putStrLn "Creando concumon"
		idConcumon <- forkIO(Concumon.run maxConcumonesSem tiempoMovConcumon mapaChan)
		threadDelay	1000000
	putStrLn "Finalizando Nido"