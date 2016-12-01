module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent
import Control.Monad
import Data.Tuple
import UtilList


-- mapaChan: (Crear(0) o Mover(1) o Borrar(2), Jugador(True) o Concumon(False), id, semaforo del jugador/concumon)
run :: Int -> Int -> Chan (Int, Bool, Int, QSem) -> MVar([Int]) -> MVar([Int]) -> IO ()
run rows cols mapaChan puntosJugadores estadoConcumones = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(rows) ++ "x" ++ show(cols) ++ "]")
	
--	let casillas = take (rows*cols) (repeat (-1))
--	let indices = take (length casillas) (iterate (1+) 0)
--	let mapaJug = zip indices casillas
--	let mapaConc = zip indices casillas

	let mapaJug = take (rows*cols) (repeat (-1))
	let mapaConc = take (rows*cols) (repeat (-1))
	let mapas = (mapaJug, mapaConc)

	putStrLn (show (UtilList.getMapCoordinates 5 rows cols))

	loopMapa mapas rows cols mapaChan puntosJugadores estadoConcumones

		

loopMapa :: ([Int], [Int]) -> Int -> Int -> Chan (Int, Bool, Int, QSem) -> MVar([Int]) -> MVar([Int]) -> IO ()
loopMapa mapas rows cols mapaChan puntosJugadores estadoConcumones = do
	accion <- readChan mapaChan

	mapasUpdated <- do {
		if (esMover accion)
			then if(esJugador accion)
				then 
					moverJugador mapas rows cols (getId accion) puntosJugadores estadoConcumones
				else 
					moverConcumon mapas (getId accion)
		else if (esCrear accion)
			then if (esJugador accion)
				then 
					crearJugador mapas rows cols (getId accion)				
				else 
					crearConcumon mapas rows cols (getId accion)
		else if (esBorrar accion)
			then if (esJugador accion)
				then
					eliminarJugador mapas rows cols (getId accion)
				else
					--Accion no habilitada para llamar desde afuera
					return mapas
			else
				--Accion incorrecta
				return mapas
	}

	let mapas = mapasUpdated

	signalQSem (getSem accion)

	loopMapa mapas rows cols mapaChan puntosJugadores estadoConcumones



findEmptySlot :: [Int] -> Int
findEmptySlot mapa = do
	let emptySlots = filterMap mapa (-1)
	if null emptySlots
		then (-1)
		else
			--Siempre se devuelve la primer posicion de las posiciones vacias. TODO: Ver como hacer que devuelva random.
			head emptySlots

--Devuelve posiciones donde el valor del mapa es == value
filterMap :: [Int] -> Int -> [Int]
filterMap mapa value = (filter (\x -> ((getValueFromMapa mapa x) == value)) [0..((length mapa)-1)])

filterAdyacentes :: [Int] -> [Int] -> [Int]
filterAdyacentes mapa adyacentes = (filter (\x -> (getValueFromMapa mapa x) == (-1)) adyacentes)


--TODO Hacer que devuelvan mapa
moverJugador :: ([Int], [Int]) -> Int -> Int -> Int -> MVar([Int]) -> MVar([Int]) -> IO ([Int], [Int])
moverJugador mapas rows cols idJugador puntosJugadores estadoConcumones = do
	putStrLn ("Intentando a mover jugador " ++ show idJugador ++ " en Mapa")
	let mapaJug = fst mapas
	let mapaConc = snd mapas
	let casillaJugador = filterMap mapaJug idJugador
	if null casillaJugador
		then do
			putStrLn("No se encontro al jugador en el mapa")
			return mapas --No se encontro al jugador en el mapaJug
		else do
			let posicionJugador = head casillaJugador
			let adyacentes = UtilList.getAdyacents posicionJugador rows cols
			let adyacentesSinJugadores = filterAdyacentes mapaJug adyacentes
			if null adyacentesSinJugadores
				then do
					putStrLn("No hay casillas adyacentes libres")
					return mapas --No hay casillas adyacentes libres
				else do
					--TODO: Agregar random para elegir la posicion!
					let nuevaPosicion = head adyacentesSinJugadores 
					putStrLn("adyacentes " ++ show adyacentes)
					putStrLn("adyacentes sin jug " ++ show adyacentesSinJugadores)
					putStrLn ("Moviendo jugador " ++ show idJugador ++ " de " ++ show (UtilList.getMapCoordinates posicionJugador rows cols) ++ " a " ++ show (UtilList.getMapCoordinates nuevaPosicion rows cols))

					let newMapaJugAux = updateElemMapa mapaJug posicionJugador (-1)
					let newMapaJug = updateElemMapa newMapaJugAux nuevaPosicion idJugador
					if (getValueFromMapa mapaConc nuevaPosicion >= 0)
						then do
							let newMapaConc = updateElemMapa mapaConc nuevaPosicion (-1)
							handleColision mapaConc nuevaPosicion idJugador puntosJugadores estadoConcumones
							return (newMapaJug, newMapaConc)
						else do
							return (newMapaJug, mapaConc)
					
					

handleColision :: [Int] -> Int -> Int -> MVar([Int]) -> MVar([Int]) -> IO()
handleColision mapaConc pos idJugador puntosJugadores estadoConcumones = do
	let idConcumon = getValueFromMapa mapaConc pos
	eliminarConcumon estadoConcumones idConcumon
	updatePoints puntosJugadores idJugador 10

moverConcumon :: ([Int], [Int]) -> Int -> IO ([Int], [Int])
moverConcumon mapas idConcumon = do
	putStrLn ("Moviendo concumon " ++ show idConcumon ++ " en Mapa")
	return mapas

crearJugador :: ([Int], [Int]) -> Int -> Int -> Int -> IO ([Int], [Int])
crearJugador mapas rows cols idJugador  = do
	--TODO: Solo checkeo en mapaJug, habria que checkear en mapaConc. (o tener otro mapa compartido...)
	let mapaJug = fst mapas
	let emptySlot = findEmptySlot mapaJug
	putStrLn ("Creando jugador " ++ show idJugador ++ " en Posicion " ++ show (UtilList.getMapCoordinates emptySlot rows cols))
	let newMapaJug = updateElemMapa mapaJug emptySlot idJugador 
	return (newMapaJug, snd mapas)


crearConcumon :: ([Int], [Int]) -> Int -> Int -> Int -> IO ([Int], [Int])
crearConcumon mapas rows cols idConcumon  = do
	let mapaJug = fst mapas
	let mapaConc = snd mapas

	putStrLn ("Creando concumon " ++ show idConcumon ++ " en Mapa")
	return mapas


eliminarConcumon:: MVar([Int]) -> Int -> IO()
eliminarConcumon estadoConcumones idConcumon = do
	putStrLn ("Eliminando Concumon " ++ show idConcumon)
	list <- takeMVar estadoConcumones
	let newList = UtilList.safeReplaceElement list idConcumon 2
	putMVar estadoConcumones newList

eliminarJugador:: ([Int], [Int]) -> Int -> Int -> Int -> IO ([Int], [Int])
eliminarJugador mapas rows cols idJugador = do
	putStrLn ("Eliminando Jugador " ++ show idJugador)
	let mapaJug = fst mapas
	let mapaConc = snd mapas
	let posJugador = head (filterMap mapaJug idJugador)
	let newMapaJug = updateElemMapa mapaJug posJugador (-1)
	return (newMapaJug, mapaConc)



esCrear :: (Int, Bool, Int, QSem) -> Bool
esCrear (accion, _, _, _) = (accion == 0)

esMover :: (Int, Bool, Int, QSem) -> Bool
esMover (accion, _, _, _) = (accion == 1)

esBorrar :: (Int, Bool, Int, QSem) -> Bool
esBorrar (accion, _, _, _) = (accion == 2)

esJugador :: (Int, Bool, Int, QSem) -> Bool
esJugador (_, jugador, _, _) = jugador

getId :: (Int, Bool, Int, QSem) -> Int
getId (_, _, id, _) = id

getSem :: (Int, Bool, Int, QSem) -> QSem
getSem (_, _, _, sem) = sem

updatePoints :: MVar([Int]) -> Int -> Int -> IO()
updatePoints listaPuntos idJugador points = do
	list <- takeMVar listaPuntos
	let actualPoints = list!!idJugador
	let newPoints = actualPoints + points
	let newList = UtilList.safeReplaceElement list idJugador newPoints
	putMVar listaPuntos newList


updateElemMapa :: [Int] -> Int -> Int -> [Int]
updateElemMapa mapa pos value = UtilList.safeReplaceElement mapa pos value


getValueFromMapa :: [Int] -> Int -> Int
getValueFromMapa mapa pos = mapa !! pos