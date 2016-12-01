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
	
	let casillas = take (rows*cols) (repeat (-1))
	let indices = take (length casillas) (iterate (1+) 0)
	let mapaJug = zip indices casillas
	let mapaConc = zip indices casillas
	let mapas = (mapaJug, mapaConc)


	forever $ do

		accion <- readChan mapaChan

		let mapas = 
			if (esMover accion)
				then do if(esJugador accion)
					then 
						moverJugador mapas rows cols (getId accion) puntosJugadores estadoConcumones
					else 
						moverConcumon mapas (getId accion)
			else if (esJugador accion)
				then 
					crearJugador mapas (getId accion)				
				else 
					crearConcumon mapas (getId accion)

		signalQSem (getSem accion)


findEmptySlot :: [(Int,Int)] -> Int
findEmptySlot mapa = do
	let emptySlots = filter ((==(-1)).snd) mapa
	if null emptySlots
		then (-1)
		else	
			--Siempre se devuelve la primer posicion de las posiciones vacias. TODO: Ver como hacer que devuelva random.
			fst (head emptySlots)



--TODO Hacer que devuelvan mapa
moverJugador :: ([(Int,Int)], [(Int,Int)]) -> Int -> Int -> Int -> MVar([Int]) -> MVar([Int]) -> ([(Int,Int)], [(Int,Int)])
moverJugador mapas rows cols idJugador puntosJugadores estadoConcumones = do
	let mapaJug = fst mapas
	let mapaConc = snd mapas
	let casillaJugador = filter ((==idJugador).snd) mapaJug
	if null casillaJugador
		then return mapas --No se encontro al jugador en el mapaJug
		else do
			let posicionJugador = fst (head casillaJugador)
			let adyacentes = UtilList.getAdyacents posicionJugador rows cols
			if null adyacentes
				then return mapas --No hay casillas adyacentes libres
				else do
					putStrLn ("Moviendo jugador " ++ show idJugador ++ " en Mapa")

					--TODO Filtrar las que tienen jugadores
					--TODO: Agregar random para elegir la posicion!
					let nuevaPosicion = head adyacentes 
					let newMapaJug = updateElemMapa (updateElemMapa mapaJug posicionJugador (-1)) nuevaPosicion idJugador
					let newMapaConc = 
						if (getValueFromMapa mapaConc nuevaPosicion >= 0)
							then
								handleColision mapaConc nuevaPosicion idJugador puntosJugadores estadoConcumones
							else
								mapaConc
					return (newMapaJug, newMapaConc)
					

handleColision :: [(Int,Int)] -> Int -> Int -> MVar([Int]) -> MVar([Int]) -> [(Int,Int)]
handleColision mapaConc pos idJugador puntosJugadores estadoConcumones = do
	let idConcumon = getValueFromMapa mapaConc pos
	eliminarConcumon estadoConcumones idConcumon
	let newMapaConc = updateElemMapa mapaConc pos (-1)
	updatePoints puntosJugadores idJugador 10
	return newMapaConc

moverConcumon :: ([(Int,Int)], [(Int,Int)]) -> Int -> ([(Int,Int)], [(Int,Int)])
moverConcumon mapas idConcumon = do
	putStrLn ("Moviendo concumon " ++ show idConcumon ++ " en Mapa")
	return mapas

eliminarConcumon:: MVar([Int]) -> Int -> IO()
eliminarConcumon estadoConcumones idConcumon = do
	putStrLn ("Eliminando Concumon " ++ show idConcumon)
	list <- takeMVar estadoConcumones
	let newList = UtilList.safeReplaceElement list idConcumon 2
	putMVar estadoConcumones newList

crearJugador :: ([(Int,Int)], [(Int,Int)]) -> Int -> ([(Int,Int)], [(Int,Int)])
crearJugador mapas idJugador  = do
	let emptySlot = findEmptySlot mapa
	putStrLn ("Creando jugador " ++ show idJugador ++ " en Posicion " ++ show emptySlot)
	return mapas


crearConcumon :: ([(Int,Int)], [(Int,Int)]) -> Int -> ([(Int,Int)], [(Int,Int)])
crearConcumon mapas idConcumon  = do
	putStrLn ("Creando concumon " ++ show idConcumon ++ " en Mapa")
	return mapas


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


updateElemMapa :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
updateElemMapa mapa pos value = UtilList.safeReplaceElement mapa pos (pos, value)


getValueFromMapa :: [(Int,Int)] -> Int -> Int
getValueFromMapa mapa pos = snd (mapa !! pos)