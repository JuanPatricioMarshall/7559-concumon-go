module Servidor
    ( Servidor.run
    ) where

import Jugador

run :: Int -> IO ()
run cantJugadores = do
	putStrLn ("Corriendo Servidor")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	putStrLn ("Agregando jugadores al juego.")
	Jugador.run