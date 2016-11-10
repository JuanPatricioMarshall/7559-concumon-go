module AdminJugadores
    ( AdminJugadores.run
    ) where

import Control.Concurrent
import Control.Monad

run :: Int -> IO ()
-- Hacer logueo de los Jugadores en un loop
run cantJugadores = do
	putStrLn ("Corriendo AdminJugadores")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	forever $ do
		putStrLn ("Logueando nuevo jugador.")
		threadDelay	5000000
		--TODO: Escribir datos de jugador en pipe