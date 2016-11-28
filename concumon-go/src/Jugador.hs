module Jugador
    ( run
    ) where

import Control.Concurrent
import Data.Tuple
import UtilList

run :: Chan (Bool, Bool, Int, QSem) -> QSem -> Int -> MVar([Bool]) -> IO ()
run mapaChan maxJugadoresSem idJugador listaIdJugadoresLibresMVar = do
	putStrLn ("Corriendo Jugador")
	jugadorSem <- newQSem 0

	let accionCrearJugador = (False, True, idJugador, jugadorSem)
	writeChan mapaChan accionCrearJugador

	waitQSem jugadorSem

	-- TODO: Agregar loop de jugar
	putStrLn ("Empezando a Jugar")
	threadDelay	10000000

	-- Actualizo Lista de Jugadores Libres - Libero ID
	UtilList.updateConcurrentList listaIdJugadoresLibresMVar idJugador True

	putStrLn ("Termino de jugar, Jugador " ++ show(idJugador))
	signalQSem maxJugadoresSem



