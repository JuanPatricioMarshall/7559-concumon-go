module Jugador
    ( run
    ) where

import Control.Concurrent


run :: Chan movimiento -> QSem -> IO ()
run movesChan jugadoresSem = do
	putStrLn ("Corriendo Jugador")
	threadDelay	20000000
	putStrLn ("Termine de jugar")
	signalQSem jugadoresSem
