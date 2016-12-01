module Concumon
    ( run
    ) where


import Control.Concurrent
import UtilList


run :: QSem -> Int -> Chan (Int, Bool, Int, QSem) -> Int -> MVar([Int])-> IO ()
run maxConcumonesSem tiempoMov mapaChan idConcumon estadoConcumonMvar = do
	putStrLn ("Iniciando concumon " ++ show idConcumon)
	concumonSem <- newQSem 0

	let accionCrearConcumon = (0, False, idConcumon, concumonSem)
	writeChan mapaChan accionCrearConcumon

	waitQSem concumonSem
	threadDelay	1000000

	estadoConcumonList <- readMVar estadoConcumonMvar
	let estadoConcumon = estadoConcumonList!!idConcumon

	--Loop Mover Concumon Hasta Muerte Concumon
	executeTask estadoConcumon idConcumon concumonSem tiempoMov mapaChan estadoConcumonMvar

	-- Actualizo Lista de Concumones Libres - Libero ID
	UtilList.updateConcurrentList estadoConcumonMvar idConcumon 0
	putStrLn ("Finalizando Concumon " ++ show(idConcumon))
	signalQSem maxConcumonesSem


	
executeTask :: Int -> Int -> QSem -> Int -> Chan (Int, Bool, Int, QSem) -> MVar([Int]) -> IO()
executeTask n idConcumon concumonSem tiempoMov mapaChan estadoConcumonMvar = do
	if n == 2
		then do
			return ()
		else do
			let accionMoverConcumon = (1, False, idConcumon, concumonSem)
			writeChan mapaChan accionMoverConcumon
			waitQSem concumonSem
			threadDelay	tiempoMov
			estadoConcumonList <- readMVar estadoConcumonMvar
			let estadoConcumon = estadoConcumonList!!idConcumon
			executeTask estadoConcumon idConcumon concumonSem tiempoMov mapaChan estadoConcumonMvar



