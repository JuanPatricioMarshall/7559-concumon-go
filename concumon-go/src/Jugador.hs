module Jugador
    ( run
    ) where

import Control.Concurrent
import Data.Tuple
import UtilList

run :: Chan (Bool, Bool, Int) -> QSem -> Int -> MVar([Bool]) -> IO ()
run mapaChan maxJugadoresSem idJugador listaIdJugadoresLibresMVar = do
	putStrLn ("Corriendo Jugador")
	jugadoresSem <- newQSem 0

	let accionCrearJugador = (False, True, idJugador)
	writeChan mapaChan accionCrearJugador

	-- TODO: waitQSem jugadoresSem

	putStrLn ("Empezando a Jugar")
	threadDelay	10000000

	-- Actualizo Lista de Jugadores Libres - Libero ID
	updateConcurrentList listaIdJugadoresLibresMVar idJugador True

	putStrLn ("Termino de jugar, Jugador " ++ show(idJugador))
	signalQSem maxJugadoresSem



updateConcurrentList :: MVar([a]) -> Int -> a -> IO()
updateConcurrentList mVar index value = do
	list <- takeMVar mVar
	let newList = UtilList.safeReplaceElement list index value
	putMVar mVar newList