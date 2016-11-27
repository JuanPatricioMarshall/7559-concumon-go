module Sysadmin
    ( run
    ) where

import Control.Concurrent
import Control.Monad

run :: MVar([Int]) -> IO ()
run listaPuntajeJugadoresMVar = do

     putStrLn ("Corriendo Sysadmin")

    --Ejemplo leer MVar
    --listaPuntajeJugadores <- readMVar listaPuntajeJugadoresMVar
    --putStrLn ("Puntaje Jugadores " ++ show(listaPuntajeJugadores))
		
