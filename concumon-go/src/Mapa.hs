module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent
import Control.Monad
import Data.Tuple
import UtilList


-- mapaChan: (Mover(True) o Crear(False), Jugador(True) o Concumon(False), id, semaforo del jugador/concumon)
run :: Int -> Int -> Chan (Bool, Bool, Int, QSem) -> MVar([Int]) -> IO ()
run x y mapaChan listaPuntajeJugadoresMVar = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(x) ++ "x" ++ show(y) ++ "]")
	forever $ do

		putStrLn ("Mapa esperando acciones")
		accion <- readChan mapaChan

		if (esMover accion)
			then do if(esJugador accion)
				then do 
					putStrLn ("Moviendo jugador " ++ show (getId accion))
					updatePoints listaPuntajeJugadoresMVar (getId accion) 10
				else do 
					putStrLn ("Moviendo concumon " ++ show (getId accion))
		else if (esJugador accion)
			then do 
				putStrLn ("Creando jugador " ++ show (getId accion))
			else do 
				putStrLn ("Creando concumon " ++ show (getId accion))

		signalQSem (getSem accion)

		--Ejemplo De Sumar Puntos
		--when (esJugador accion) $ updatePoints listaPuntajeJugadoresMVar (getId accion) 10


		

esMover :: (Bool, Bool, Int, QSem) -> Bool
esMover (mover, _, _, _) = mover

esJugador :: (Bool, Bool, Int, QSem) -> Bool
esJugador (_, jugador, _, _) = jugador

getId :: (Bool, Bool, Int, QSem) -> Int
getId (_, _, id, _) = id

getSem :: (Bool, Bool, Int, QSem) -> QSem
getSem (_, _, _, sem) = sem

updatePoints :: MVar([Int]) -> Int -> Int -> IO()
updatePoints listaPuntos idJugador points = do
	list <- takeMVar listaPuntos
	let actualPoints = list!!idJugador
	let newPoints = actualPoints + points
	let newList = UtilList.safeReplaceElement list idJugador newPoints
	putMVar listaPuntos newList