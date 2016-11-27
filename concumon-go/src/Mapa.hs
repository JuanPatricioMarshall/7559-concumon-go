module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent
import Control.Monad


run :: Int -> Int -> Chan String -> IO ()
run x y mapaChan = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(x) ++ "x" ++ show(y) ++ "]")
	forever $ do

		putStrLn ("Mapa esperando acciones")
		accion <- readChan mapaChan
		putStrLn accion

		--TODO: signalQSem del que mando la accion
			
