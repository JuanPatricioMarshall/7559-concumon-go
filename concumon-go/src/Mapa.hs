module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent
import Control.Monad
import Data.Tuple
import UtilList


-- mapaChan: (Mover(True) o Crear(False), Jugador(True) o Concumon(False), id, semaforo del jugador/concumon)
run :: Int -> Int -> Chan (Bool, Bool, Int, QSem) -> MVar([Int]) -> IO ()
run x y mapaChan puntosJugadores = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(x) ++ "x" ++ show(y) ++ "]")
	forever $ do

		putStrLn ("Mapa esperando acciones")
		accion <- readChan mapaChan

		if (esMover accion)
			then do if(esJugador accion)
				then do 
					moverJugador (getId accion) puntosJugadores
				else do 
					moverConcumon (getId accion)
		else if (esJugador accion)
			then do 
				crearJugador (getId accion)				
			else do 
				crearConcumon (getId accion)

		signalQSem (getSem accion)


moverJugador :: Int  -> MVar([Int]) -> IO()
moverJugador idJugador puntosJugadores = do
	putStrLn ("Moviendo jugador " ++ show idJugador)
	updatePoints puntosJugadores idJugador 10

moverConcumon :: Int -> IO()
moverConcumon idConcumon = do
	putStrLn ("Moviendo concumon " ++ show idConcumon)

crearJugador :: Int -> IO()
crearJugador idJugador  = do
	putStrLn ("Creando jugador " ++ show idJugador)

crearConcumon :: Int -> IO()
crearConcumon idConcumon  = do
	putStrLn ("Creando concumon " ++ show idConcumon)


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