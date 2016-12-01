module Jugador
    ( run
    ) where

import Control.Concurrent
import Data.Tuple
import UtilList


run :: Chan (Int, Bool, Int, QSem) -> QSem -> Int -> MVar([Bool]) -> IO ()
run mapaChan maxJugadoresSem idJugador listaIdJugadoresLibresMVar = do
	putStrLn ("Corriendo Jugador")
	jugadorSem <- newQSem 0

	let accionCrearJugador = (0, True, idJugador, jugadorSem)
	writeChan mapaChan accionCrearJugador

	waitQSem jugadorSem

	putStrLn ("Empezando a Jugar")
	executeTask 1 idJugador jugadorSem mapaChan


	--Mensaje a mapa para que me libere del mapa
	let accionBorrarJugador = (2, True, idJugador, jugadorSem)
	writeChan mapaChan accionBorrarJugador
	waitQSem jugadorSem

	-- Actualizo Lista de Jugadores Libres - Libero ID
	UtilList.updateConcurrentList listaIdJugadoresLibresMVar idJugador True

	putStrLn ("Termino de jugar, Jugador " ++ show(idJugador))
	signalQSem maxJugadoresSem


executeTask :: Int -> Int -> QSem -> Chan (Int, Bool, Int, QSem) -> IO()
executeTask n idJugador jugadorSem mapaChan = do
	if n == 0
		then do
			return ()
		else do
				let accionMoverJugador = (1, True, idJugador, jugadorSem)
				writeChan mapaChan accionMoverJugador
				waitQSem jugadorSem
				threadDelay	10000000 -- TODO Random
				let rnd = 0 -- TODO Random
				executeTask rnd idJugador jugadorSem mapaChan 


