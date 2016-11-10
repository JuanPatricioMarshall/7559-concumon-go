module Servidor
    ( Servidor.run
    ) where

import Control.Concurrent
import Control.Monad

import Jugador

run :: Int -> Chan String -> Chan String -> QSem ->  IO ()
run cantJugadores connectionChan mapaChan maxJugadoresSem = do
	putStrLn ("Corriendo Servidor")
	forever $ do
		waitQSem maxJugadoresSem
		line <- readChan connectionChan
		putStrLn line
		putStrLn ("Agregando nuevo jugador.")
		idJugador <- forkIO(Jugador.run mapaChan maxJugadoresSem)
		putStrLn "Se agrego un nuevo jugador"
	putStrLn "Finalizando servidor"