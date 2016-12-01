module Jugador
    ( run
    ) where

import Control.Concurrent
import Data.Tuple
import UtilList
import UtilRandom


run :: Chan (Int, Bool, Int, QSem) -> QSem -> Int -> MVar([Bool]) -> MVar([Int]) -> IO ()
run mapaChan maxJugadoresSem idJugador listaIdJugadoresLibresMVar puntosJugadores = do
	putStrLn ("Iniciando jugador " ++ show idJugador)
	jugadorSem <- newQSem 0

	let accionCrearJugador = (0, True, idJugador, jugadorSem)
	writeChan mapaChan accionCrearJugador
		
	waitQSem jugadorSem

	putStrLn ("Jugador " ++ show idJugador ++ " empezando a jugar")


	iteraciones <- UtilRandom.getRandomElem [2,3,4,5]
	
	executeTask iteraciones idJugador jugadorSem mapaChan


	--Mensaje a mapa para que me libere del mapa
	let accionBorrarJugador = (2, True, idJugador, jugadorSem)
	writeChan mapaChan accionBorrarJugador
	waitQSem jugadorSem

	--Poniendo en cero los puntos del jugador antes de irse
	UtilList.updateConcurrentList puntosJugadores idJugador 0

	-- Actualizo Lista de Jugadores Libres - Libero ID
	UtilList.updateConcurrentList listaIdJugadoresLibresMVar idJugador True

	putStrLn ("Termino de jugar el Jugador " ++ show(idJugador))
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

			tiempo <- UtilRandom.getRandomInt 2000000 10000000
			
			threadDelay	tiempo
			executeTask (n-1) idJugador jugadorSem mapaChan 


