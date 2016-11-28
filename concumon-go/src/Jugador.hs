module Jugador
    ( run
    ) where

import Control.Concurrent
import Data.Tuple
import UtilList

run :: Chan (Bool, Bool, Int) -> QSem -> MVar([Bool]) -> IO ()
run mapaChan maxJugadoresSem listaIdJugadoresLibresMVar = do
	putStrLn ("Corriendo Jugador")
	jugadoresSem <- newQSem 0

	-- Actualizo Lista de Jugadores Libres - Asigno ID
	listaIdJugadoresLibres <- takeMVar listaIdJugadoresLibresMVar
	let idJugador = UtilList.getIndexOfFirstBoolEqualTo listaIdJugadoresLibres True
	let newListaIdJugadoresLibres = UtilList.safeReplaceElement listaIdJugadoresLibres idJugador False
	putMVar listaIdJugadoresLibresMVar newListaIdJugadoresLibres


	let accionCrearJugador = (False, True, idJugador)
	writeChan mapaChan accionCrearJugador

	-- TODO: waitQSem jugadoresSem

	putStrLn ("Empezando a Jugar")
	threadDelay	10000000

		-- Actualizo Lista de Jugadores Libres - Libero ID
	listaIdJugadoresLibres <- takeMVar listaIdJugadoresLibresMVar
	let newListaIdJugadoresLibres = UtilList.safeReplaceElement listaIdJugadoresLibres idJugador True
	putMVar listaIdJugadoresLibresMVar newListaIdJugadoresLibres

	putStrLn ("Termino de jugar, Jugador " ++ show(idJugador))
	signalQSem maxJugadoresSem
