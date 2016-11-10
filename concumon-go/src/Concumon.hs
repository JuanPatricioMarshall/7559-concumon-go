module Concumon
    ( run
    ) where

run :: Int -> IO ()
run tiempoMov = do
	putStrLn ("Corriendo Concumon")
	putStrLn ("Tiempo de movimiento: " ++ show(tiempoMov))
	