module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent
import Control.Monad
import Data.Tuple
import UtilList


-- mapaChan: (Mover(True) o Crear(False), Jugador(True) o Concumon(False), id, semaforo del jugador/concumon)
run :: Int -> Int -> Chan (Bool, Bool, Int, QSem) -> MVar([Int]) -> MVar([Int]) -> IO ()
run rows cols mapaChan puntosJugadores estadoConcumones = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(rows) ++ "x" ++ show(cols) ++ "]")
	
	let casillas = take (rows*cols) (repeat 0)
	let indices = take (length casillas) (iterate (1+) 0)
	let mapa = zip indices casillas

	forever $ do

		accion <- readChan mapaChan

		if (esMover accion)
			then do if(esJugador accion)
				then do 
					moverJugador mapa rows cols (getId accion) puntosJugadores
				else do 
					moverConcumon (getId accion)
		else if (esJugador accion)
			then do 
				crearJugador mapa (getId accion)				
			else do 
				crearConcumon (getId accion)

		signalQSem (getSem accion)


findEmptySlot :: [(Int,Int)] -> Int
findEmptySlot mapa = do
	let emptySlots = filter ((==0).snd) mapa
	if null emptySlots
		then (-1)
		else	
			--Siempre se devuelve la primer posicion de las posiciones vacias. TODO: Ver como hacer que devuelva random.
			fst (head emptySlots)




moverJugador :: [(Int,Int)] -> Int -> Int -> Int -> MVar([Int]) -> IO()
moverJugador mapa rows cols idJugador puntosJugadores = do

	let casillaJugador = filter ((==idJugador).snd) mapa
	if null casillaJugador
		then return() --No se encontro al jugador en el mapa
		else do
			let posicionJugador = fst (head casillaJugador)
			let adyacentes = UtilList.getAdyacents posicionJugador rows cols
			if null adyacentes
				then return() --No hay casillas adyacentes libres
				else do
					putStrLn ("Moviendo jugador " ++ show idJugador ++ " en Mapa")

					--TODO: Agregar random para elegir la posicion!
					let nuevaPosicion = head adyacentes

					handleColision

handleColision :: IO()
	updatePoints puntosJugadores idJugador 10



moverConcumon :: Int -> IO()
moverConcumon idConcumon = do
	putStrLn ("Moviendo concumon " ++ show idConcumon ++ " en Mapa")

eliminarConcumon:: MVar([Int]) -> Int -> IO()
eliminarConcumon estadoConcumones idConcumon = do
	putStrLn ("Eliminando Concumon " ++ show idConcumon)
	list <- takeMVar estadoConcumones
	let newList = UtilList.safeReplaceElement list idConcumon 2
	putMVar estadoConcumones newList

crearJugador :: [(Int,Int)] -> Int -> IO()
crearJugador mapa idJugador  = do
	let emptySlot = findEmptySlot mapa
	putStrLn ("Creando jugador " ++ show idJugador ++ " en Posicion " ++ show emptySlot)


crearConcumon :: Int -> IO()
crearConcumon idConcumon  = do
	putStrLn ("Creando concumon " ++ show idConcumon ++ " en Mapa")


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