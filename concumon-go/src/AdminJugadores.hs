module AdminJugadores
    ( AdminJugadores.run
    ) where

run :: Int -> IO ()
-- Hacer logueo de los Jugadores en un while
run cantJugadores = do
	putStrLn ("Corriendo AdminJugadores")
	putStrLn ("Cant jugadores: " ++ (show cantJugadores))
	putStrLn ("Logueando jugadores.")