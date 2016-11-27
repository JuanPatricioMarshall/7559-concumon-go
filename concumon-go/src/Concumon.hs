module Concumon
    ( run
    ) where


import Control.Concurrent


run :: QSem -> Int -> Chan (Bool, Bool, Int) -> IO ()
run maxConcumonesSem tiempoMov mapaChan = do
	putStrLn ("Corriendo Concumon")
	concumonSem <- newQSem 0

	-- TODO: Obtener id del concumon
	let id = 1
	writeChan mapaChan (False, False, id)

	-- TODO: waitQSem concumonSem
	threadDelay	1000000
	putStrLn ("Soy un concumon en el mapa")
	threadDelay	10000000
	putStrLn ("Finalizando concumon")
	signalQSem maxConcumonesSem
	

