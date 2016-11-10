module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent


run :: Int -> Int -> Chan movimiento -> IO ()
run x y movesChan = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(x) ++ "x" ++ show(y) ++ "]")
	putStrLn ("Mapa esperando acciones")
	