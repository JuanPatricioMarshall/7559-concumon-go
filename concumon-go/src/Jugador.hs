module Jugador
    ( run
    ) where

import Control.Concurrent


run :: Chan movimiento ->  IO ()
run movesChan = do
	putStrLn ("Corriendo Jugador")