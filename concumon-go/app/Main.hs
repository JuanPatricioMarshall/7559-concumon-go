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
	let maxJugadores = 10
	let maxConcumones = 8

	putStrLn("Parametros: ")
	putStrLn("Dimension X: " ++ show(xDim))
	putStrLn("Dimension Y: " ++ show(yDim))
	putStrLn("Tiempo Movimiento Concumon: " ++ show(tiempoMovConcumon))
	putStrLn("Cantidad total de jugadores: " ++ show(cantJugadores))
	putStrLn("Cantidad maxima de jugadores en mapa: " ++ show(maxJugadores))
	putStrLn("Cantidad maxima de concumones en mapa: " ++ show(maxConcumones))
	putStrLn("")

	idAdminJugadores <- forkIO (AdminJugadores.run cantJugadores)
	idServidor <- forkIO (Servidor.run cantJugadores)
	idNido <- forkIO (Nido.run tiempoMovConcumon)
	idMapa <- forkIO (Mapa.run xDim yDim)
	idSysadmin <- forkIO (Sysadmin.run)


	threadDelay	10000000
	putStrLn("finalizando ejecucion")