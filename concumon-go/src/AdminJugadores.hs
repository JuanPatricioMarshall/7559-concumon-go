module AdminJugadores
    ( AdminJugadores.run
    ) where

import Control.Concurrent
import Control.Monad

run :: Int -> Chan String -> IO ()
run cantJugadores connectionChan = do
	putStrLn ("Corriendo AdminJugadores")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	loginNPlayer cantJugadores connectionChan

loginNPlayer :: Int -> Chan String -> IO()
loginNPlayer n connectionChan = do
	if n == 0
		then do
			return ()
		else do
				threadDelay	1000000
				putStrLn ("Logueando Jugador " ++ show (n))
				writeChan connectionChan "Logueando nuevo jugador."
				loginNPlayer (n-1) connectionChan


  	