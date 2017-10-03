module Sysadmin
    ( run
    ) where

import Control.Concurrent
import Control.Monad

run :: MVar([Int]) -> IO ()
run listaPuntajeJugadoresMVar = do

     putStrLn ("Corriendo Sysadmin")
     forever $ do
  		  listaPuntajeJugadores <- readMVar listaPuntajeJugadoresMVar
   		  putStrLn ("Puntaje Jugadores " ++ show(listaPuntajeJugadores))
   		  threadDelay	5000000
		
