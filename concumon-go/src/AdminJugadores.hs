module AdminJugadores
    ( AdminJugadores.run
    ) where

import Control.Concurrent
import Control.Monad

run :: Int -> Chan String -> IO ()
-- Hacer logueo de los Jugadores en un loop
run cantJugadores connectionChan = do
	putStrLn ("Corriendo AdminJugadores")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	forever $ do
		putStrLn ("Logueando nuevo jugador.")
		threadDelay	5000000
		writeChan connectionChan "Hola, entre. Soy jugador nuevo"