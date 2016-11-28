module Nido
    ( Nido.run
    ) where

import Control.Concurrent
import Control.Monad
import UtilList

import Concumon

run :: QSem -> Int -> Chan (Bool, Bool, Int) -> MVar([Bool]) -> IO ()
run maxConcumonesSem tiempoMovConcumon mapaChan listaIdConcumonesLibresMvar = do
	putStrLn ("Corriendo Nido")
	-- TODO: Ver si esta bien con forever, o usar otra cosa
	forever $ do
		waitQSem maxConcumonesSem
		putStrLn "Creando concumon"

		-- Actualizo Lista de Concumones Libres - Asigno ID
		listaIdConcumonesLibres <- takeMVar listaIdConcumonesLibresMvar
		let idConcumon = UtilList.getIndexOfFirstBoolEqualTo listaIdConcumonesLibres True
		let newListaIdConcumonesLibres = UtilList.safeReplaceElement listaIdConcumonesLibres idConcumon False
		putMVar listaIdConcumonesLibresMvar newListaIdConcumonesLibres


		idConcumon <- forkIO(Concumon.run maxConcumonesSem tiempoMovConcumon mapaChan idConcumon listaIdConcumonesLibresMvar)
		threadDelay	1000000
	putStrLn "Finalizando Nido"
