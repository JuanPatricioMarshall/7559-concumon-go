module Concumon
    ( run
    ) where


import Control.Concurrent


run :: QSem -> Int -> Chan String -> IO ()
run maxConcumonesSem tiempoMov mapaChan = do
	putStrLn ("Corriendo Concumon")
	concumonSem <- newQSem 0

	-- TODO: Mandar a mapa el id y el semaforo del concumon

	writeChan mapaChan "Agregando concumon a Mapa"

	-- TODO: waitQSem concumonSem
	threadDelay	1000000
	putStrLn ("Soy un concumon en el mapa")
	threadDelay	10000000
	putStrLn ("Finalizando concumon")
	signalQSem maxConcumonesSem
	

