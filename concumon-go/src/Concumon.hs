module Concumon
    ( run
    ) where


import Control.Concurrent


run :: Int -> Chan movimiento -> IO ()
run tiempoMov movesChan = do
	putStrLn ("Corriendo Concumon")
	putStrLn ("Tiempo de movimiento: " ++ show(tiempoMov))
	