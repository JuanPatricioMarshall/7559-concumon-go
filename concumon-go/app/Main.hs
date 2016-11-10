module Main where

import AdminJugadores
import Servidor
import Nido
import Mapa
import Sysadmin

main :: IO ()
main = do
	let cantJugadores = 5
	let xDim = 50
	let yDim = 50
	AdminJugadores.run cantJugadores
	Servidor.run cantJugadores
	Nido.run
	Mapa.run xDim yDim
	Sysadmin.run