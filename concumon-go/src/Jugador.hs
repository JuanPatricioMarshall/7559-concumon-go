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

	-- TODO: Agregar loop de jugar
	putStrLn ("Empezando a Jugar")
	threadDelay	10000000

	-- Actualizo Lista de Jugadores Libres - Libero ID
	UtilList.updateConcurrentList listaIdJugadoresLibresMVar idJugador True

	putStrLn ("Termino de jugar, Jugador " ++ show(idJugador))
	signalQSem maxJugadoresSem



