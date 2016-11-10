module Main where

import Jugador
import Concumon

main :: IO ()
main = do
	Jugador.run
	Concumon.run