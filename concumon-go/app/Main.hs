module Main where

import Control.Concurrent

import AdminJugadores
import Servidor
import Nido
import Mapa
import Sysadmin

main :: IO ()
main = do
	let xDim = 50
	let yDim = 50
	let tiempoMovConcumon = 5
	let cantJugadores = 100
	let maxJugadores = 3
	let maxConcumones = 3

	putStrLn("Parametros: ")
	putStrLn("Dimension X: " ++ show(xDim))
	putStrLn("Dimension Y: " ++ show(yDim))
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
	idServidor <- forkIO (Servidor.run cantJugadores connectionChan mapaChan maxJugadoresSem)
	idNido <- forkIO (Nido.run maxConcumonesSem tiempoMovConcumon mapaChan)
	idMapa <- forkIO (Mapa.run xDim yDim mapaChan)
	idSysadmin <- forkIO (Sysadmin.run)


	putStrLn("Presione Enter para finalizar ejecucion")
	getLine
	putStrLn("finalizando ejecucion")