module Jugador
    ( run
    ) where

import Control.Concurrent
import Data.Tuple

run :: Chan (Bool, Bool, Int) -> QSem -> IO ()
run mapaChan maxJugadoresSem = do
	putStrLn ("Corriendo Jugador")
	jugadoresSem <- newQSem 0

	-- TODO: Obtener id del jugador

	let id = 1
	let accionCrearJugador = (False, True, id)
	writeChan mapaChan accionCrearJugador

	-- TODO: waitQSem jugadoresSem

	putStrLn ("Empezando a Jugar")
	threadDelay	10000000

	putStrLn ("Termine de jugar")
	signalQSem maxJugadoresSem
