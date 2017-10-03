module Main where

import Control.Concurrent

import AdminJugadores
import Servidor
import Nido
import Mapa
import Sysadmin
import UtilFile
import System.Random

main :: IO ()
main = do

	n <- randomRIO (0, 100 :: Int)
	putStrLn (show n)

	file <- readFile "config.txt"
	fileList <- UtilFile.fileToList file

	let rows = UtilFile.getParameter fileList "height" 50
	let cols = UtilFile.getParameter fileList "width" 50
	let tiempoMovConcumon = UtilFile.getParameter fileList "timeConcumon" 50
	let maxConcumones = UtilFile.getParameter fileList "maxConcumones" 3
	let maxJugadores = UtilFile.getParameter fileList "maxJugadores" 3

	let cantJugadores = 100

	-- Creo Listas de Jugadores Libres
	let listaIdJugadoresLibres = take (maxJugadores) (repeat True)	
	listaIdJugadoresLibresMVar <- newEmptyMVar
	putMVar listaIdJugadoresLibresMVar listaIdJugadoresLibres

	-- Creo Listas de Puntaje de Jugadores
	let listaPuntajeJugadores = take (maxJugadores) (repeat 0)
	listaPuntajeJugadoresMVar <- newEmptyMVar
	putMVar listaPuntajeJugadoresMVar listaPuntajeJugadores

	-- Listas de Estado Concumon { 0 Free, 1 Live, 2 Dead}
	let estadoConumones = take (maxConcumones) (repeat 0)	
	estadoConumonesMvar <- newEmptyMVar
	putMVar estadoConumonesMvar estadoConumones

	putStrLn("Parametros: ")
	putStrLn("Filas: " ++ show(rows))
	putStrLn("Columnas: " ++ show(cols))
	putStrLn("Tiempo Movimiento Concumon: " ++ show(tiempoMovConcumon))
	putStrLn("Cantidad total de jugadores: " ++ show(cantJugadores))
	putStrLn("Cantidad maxima de jugadores en mapa: " ++ show(maxJugadores))
	putStrLn("Cantidad maxima de concumones en mapa: " ++ show(maxConcumones))

	putStrLn("")

	connectionChan <- newChan
	mapaChan <- newChan
	maxJugadoresSem <- newQSem maxJugadores
	maxConcumonesSem <- newQSem maxConcumones

	idAdminJugadores <- forkIO (AdminJugadores.run cantJugadores connectionChan)
	idServidor <- forkIO (Servidor.run cantJugadores connectionChan mapaChan maxJugadoresSem listaIdJugadoresLibresMVar listaPuntajeJugadoresMVar)
	idNido <- forkIO (Nido.run maxConcumonesSem tiempoMovConcumon mapaChan estadoConumonesMvar)
	idMapa <- forkIO (Mapa.run rows cols mapaChan listaPuntajeJugadoresMVar estadoConumonesMvar)
	idSysadmin <- forkIO (Sysadmin.run listaPuntajeJugadoresMVar)


	putStrLn("Presione Enter para finalizar ejecucion")
	getLine
	putStrLn("Finalizando ejecucion")
