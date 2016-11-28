module Concumon
    ( run
    ) where


import Control.Concurrent
import UtilList


run :: QSem -> Int -> Chan (Bool, Bool, Int, QSem) -> Int -> MVar([Bool])-> IO ()
run maxConcumonesSem tiempoMov mapaChan idConcumon listaIdConcumonesLibresMvar = do
	putStrLn ("Corriendo Concumon")
	concumonSem <- newQSem 0

	let accionCrearConcumon = (False, False, idConcumon, concumonSem)
	writeChan mapaChan accionCrearConcumon

	waitQSem concumonSem
	threadDelay	1000000
	putStrLn ("Soy un concumon en el mapa")

	-- TODO: Hacer loop de moverse en el mapa hasta que le llegue la senial de captura
	threadDelay	10000000

	-- Actualizo Lista de Concumones Libres - Libero ID
	UtilList.updateConcurrentList listaIdConcumonesLibresMvar idConcumon True
	
	putStrLn ("Finalizando Concumon " ++ show(idConcumon))
	signalQSem maxConcumonesSem
	

