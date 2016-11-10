module Nido
    ( Nido.run
    ) where

import Concumon

run :: IO ()
run = do
	putStrLn ("Corriendo Nido")
	putStrLn "Creando concumones"
	Concumon.run