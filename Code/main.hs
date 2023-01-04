-- https://hackage.haskell.org/package/gloss

-- in C:\Program Files (x86)\WinHugs\packages
-- git clone https://gitlab.haskell.org/ghc/ghc.git
-- git clone https://github.com/benl23x5/gloss

-- import Graphics.Gloss

import TicTacToe.HumanVSMachine
import TicTacToe.HumanVSHuman
import TicTacToe.MachineVSMachine

main :: IO ()
main = do
	putStrLn $ "*******************************************************"
	putStrLn $ "Implementacion Algoritmo MinMax en Haskell"
	putStrLn $ "Por: Alba Marquez Rodriguez y Alberto Fernandez Merchan"
	putStrLn $ "*******************************************************"
	putStrLn $ "Elige una opcion"
	putStrLn $ "1. Humano vs. Humano"
	putStrLn $ "2. Humano vs. Maquina"
	putStrLn $ "3. Maquina vs. Maquina"
	putStrLn $ "4. Salir"
	putStr $ " Escribe la opcion: -> "
	opc <- getLine
	case opc of
		"1" -> humanvshuman
		"2" -> humanvsmachine
		"3" -> machinevsmachine
		"4" -> putStrLn $ "Saliendo..."
		otherwise -> main
		
	if (opc /= "4") then
		main
	else 
		putStrLn $ "Fin"