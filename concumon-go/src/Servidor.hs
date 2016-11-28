module Servidor
    ( Servidor.run
    ) where

import Control.Concurrent
import Control.Monad

import Jugador
import UtilList

run :: Int -> Chan String -> Chan (Bool, Bool, Int) -> QSem -> MVar([Bool]) -> IO ()
run cantJugadores connectionChan mapaChan maxJugadoresSem listaIdJugadoresLibresMVar  = do
	putStrLn ("Corriendo Servidor")
	forever $ do		
		waitQSem maxJugadoresSem
		line <- readChan connectionChan
		putStrLn line
		putStrLn ("Agregando nuevo jugador.")
		idJugador <- forkIO(Jugador.run mapaChan maxJugadoresSem listaIdJugadoresLibresMVar)
		putStrLn "Se agrego un nuevo jugador"
		putStrLn "Finalizando servidor"
