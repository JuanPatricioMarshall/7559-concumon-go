module Servidor
    ( Servidor.run
    ) where

import Control.Concurrent
import Control.Monad

import Jugador
import UtilList

run :: Int -> Chan String -> Chan (Int, Bool, Int, QSem) -> QSem -> MVar([Bool]) -> MVar([Int]) -> IO ()
run cantJugadores connectionChan mapaChan maxJugadoresSem listaIdJugadoresLibresMVar puntosJugadores = do
	putStrLn ("Corriendo Servidor")
	forever $ do		
		waitQSem maxJugadoresSem
		line <- readChan connectionChan

		-- Actualizo Lista de Jugadores Libres - Asigno ID
		listaIdJugadoresLibres <- takeMVar listaIdJugadoresLibresMVar
		let idJugador = UtilList.getIndexOfFirstBoolEqualTo listaIdJugadoresLibres True
		let newListaIdJugadoresLibres = UtilList.safeReplaceElement listaIdJugadoresLibres idJugador False
		putMVar listaIdJugadoresLibresMVar newListaIdJugadoresLibres

		putStrLn line
		putStrLn ("Server: Agregando nuevo jugador.")
		forkIO(Jugador.run mapaChan maxJugadoresSem idJugador listaIdJugadoresLibresMVar puntosJugadores)
	putStrLn "Finalizando servidor"
