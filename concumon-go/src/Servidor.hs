module Servidor
    ( Servidor.run
    ) where

import Control.Concurrent
import Control.Monad

import Jugador

run :: Int -> IO ()
run cantJugadores = do
	putStrLn ("Corriendo Servidor")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	putStrLn ("Agregando jugadores al juego.")
	forever $ do
		-- TODO: Leer datos de jugador de pipe
		putStrLn ("Agregando nuevo jugador.")
		idJugador <- forkIO(Jugador.run)
		threadDelay	5000000
	putStrLn "Finalizando Servidor"