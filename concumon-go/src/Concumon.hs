module Concumon
    ( run
    ) where


import Control.Concurrent
import UtilList


run :: QSem -> Int -> Chan (Bool, Bool, Int) -> Int -> MVar([Bool])-> IO ()
run maxConcumonesSem tiempoMov mapaChan idConcumon listaIdConcumonesLibresMvar = do
	putStrLn ("Corriendo Concumon")
	concumonSem <- newQSem 0

	let accionCrearConcumon = (False, False, idConcumon)
	writeChan mapaChan accionCrearConcumon

	-- TODO: waitQSem concumonSem
	threadDelay	1000000
	putStrLn ("Soy un concumon en el mapa")
	threadDelay	10000000

	-- Actualizo Lista de Concumones Libres - Libero ID
	UtilList.updateConcurrentList listaIdConcumonesLibresMvar idConcumon True
	
	putStrLn ("Finalizando Concumon " ++ show(idConcumon))
	signalQSem maxConcumonesSem
	

