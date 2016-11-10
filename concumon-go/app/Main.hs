module Main where

import AdminJugadores
import Servidor

main :: IO ()
main = do
	let cantJugadores = 5
	AdminJugadores.run cantJugadores
	Servidor.run cantJugadores