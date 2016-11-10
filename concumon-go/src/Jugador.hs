module Jugador
    ( run
    ) where

import Control.Concurrent


run :: Chan String -> QSem -> IO ()
run mapaChan maxJugadoresSem = do
	putStrLn ("Corriendo Jugador")
	jugadoresSem <- newQSem 0

	-- TODO: Mandar a mapa el id y el semaforo del jugador

	writeChan mapaChan "Agregando jugador a Mapa"

	-- TODO: waitQSem jugadoresSem

	putStrLn ("Empezando a Jugar")
	threadDelay	10000000

	putStrLn ("Termine de jugar")
	signalQSem maxJugadoresSem
