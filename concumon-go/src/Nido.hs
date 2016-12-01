module Nido
    ( Nido.run
    ) where

import Control.Concurrent
import Control.Monad
import UtilList

import Concumon

run :: QSem -> Int -> Chan (Int, Bool, Int, QSem) -> MVar([Int]) -> IO ()
run maxConcumonesSem tiempoMovConcumon mapaChan estadoConcumonesMvar = do
	putStrLn ("Corriendo Nido")
	-- TODO: Ver si esta bien con forever, o usar otra cosa
	forever $ do
		waitQSem maxConcumonesSem
		putStrLn "Creando concumon"

		-- Actualizo Lista de Concumones Libres - Asigno ID  { 0 Free, 1 Live, 2 Dead}
		estadoConcumones <- takeMVar estadoConcumonesMvar
		let idConcumon = UtilList.getIndexOfFirstIntEqualTo estadoConcumones 0
		let newEstadoConcumones = UtilList.safeReplaceElement estadoConcumones idConcumon 1
		putMVar estadoConcumonesMvar newEstadoConcumones


		idConcumon <- forkIO(Concumon.run maxConcumonesSem tiempoMovConcumon mapaChan idConcumon estadoConcumonesMvar)
		threadDelay	1000000
	putStrLn "Finalizando Nido"
