module Mapa
    ( Mapa.run
    ) where

import Control.Concurrent
import Control.Monad
import Data.Tuple


-- mapaChan: (Mover(True) o Crear(False), Jugador(True) o Concumon(False), id)
run :: Int -> Int -> Chan (Bool, Bool, Int) -> IO ()
run x y mapaChan = do
	putStrLn ("Corriendo Mapa")
	putStrLn ("Dimensiones: [" ++ show(x) ++ "x" ++ show(y) ++ "]")
	forever $ do
		putStrLn ("Mapa esperando acciones")
		accion <- readChan mapaChan
		putStrLn (show (esMover accion))
		putStrLn (show (esJugador accion))
		putStrLn (show (getId accion))

		--TODO: signalQSem del que mando la accion


esMover :: (Bool, Bool, Int) -> Bool
esMover (mover, jugador, id) = mover

esJugador :: (Bool, Bool, Int) -> Bool
esJugador (mover, jugador, id) = jugador


getId :: (Bool, Bool, Int) -> Int
getId (mover, jugador, id) = id