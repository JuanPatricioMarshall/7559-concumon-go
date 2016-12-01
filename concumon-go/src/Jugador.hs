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

	putStrLn ("Empezando a Jugar")
	executeTask 1 idJugador jugadorSem mapaChan

	-- Actualizo Lista de Jugadores Libres - Libero ID
	UtilList.updateConcurrentList listaIdJugadoresLibresMVar idJugador True

	putStrLn ("Termino de jugar, Jugador " ++ show(idJugador))
	signalQSem maxJugadoresSem


executeTask :: Int -> Int -> QSem -> Chan (Bool, Bool, Int, QSem) -> IO()
executeTask n idJugador jugadorSem mapaChan = do
	if n == 0
		then do
			return ()
		else do
				let accionMoverJugador = (True, True, idJugador, jugadorSem)
				writeChan mapaChan accionMoverJugador
				waitQSem jugadorSem
				threadDelay	10000000 -- TODO Random
				let rnd = 0 -- TODO Random
				executeTask rnd idJugador jugadorSem mapaChan 


