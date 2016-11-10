module Mapa
    ( Mapa.run
    ) where

run :: Int -> Int -> IO ()
run x y = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(x) ++ "x" ++ show(y) ++ "]")
	putStrLn ("Mapa esperando acciones")
	