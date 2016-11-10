module Servidor
    ( Servidor.run
    ) where

import Control.Concurrent
import Control.Monad

import Jugador

run :: Int -> Chan String -> Chan movimiento -> QSem ->  IO ()
run cantJugadores connectionChan movesChan jugadoresSem = do
	putStrLn ("Corriendo Servidor")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	putStrLn ("Agregando jugadores al juego.")
	forever $ do
		waitQSem jugadoresSem
		line <- readChan connectionChan
		putStrLn line
		putStrLn ("Agregando nuevo jugador.")
		idJugador <- forkIO(Jugador.run movesChan jugadoresSem)
		putStrLn "Se agrego un nuevo jugador"
	putStrLn "Finalizando servidor"