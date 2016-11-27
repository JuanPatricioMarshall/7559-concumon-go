module Servidor
    ( Servidor.run
    ) where

import Control.Concurrent
import Control.Monad

import Jugador
import UtilList

run :: Int -> Chan String -> Chan (Bool, Bool, Int) -> QSem -> MVar([Bool]) -> MVar([Int]) -> IO ()
run cantJugadores connectionChan mapaChan maxJugadoresSem listaIdJugadoresLibresMVar listaPuntajeJugadoresMVar  = do
	putStrLn ("Corriendo Servidor")
	forever $ do

		--Ejemplo Modificar Valores en MVae
		-- listaIdJugadoresLibres <- takeMVar listaIdJugadoresLibresMVar
		-- let newListaIdJugadoresLibres = UtilList.safeReplaceElement listaIdJugadoresLibres 0 False
		-- putMVar listaIdJugadoresLibresMVar newListaIdJugadoresLibres

		waitQSem maxJugadoresSem

		line <- readChan connectionChan
		putStrLn line
		putStrLn ("Agregando nuevo jugador.")
		idJugador <- forkIO(Jugador.run mapaChan maxJugadoresSem)
		putStrLn "Se agrego un nuevo jugador"
		putStrLn "Finalizando servidor"
