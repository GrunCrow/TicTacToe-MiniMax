module TicTacToe.HumanVSHuman where

import TicTacToe.Objects
import TicTacToe.DisplayBoard
import TicTacToe.BoardFunctions
import TicTacToe.GameRules
-- ============================================================================
--			Human vs Human
-- ============================================================================
-- run: display current board state
run :: Board -> Player -> IO ()
run board player = do 
						putBoard board -- displays board
						run' board player


-- checks if any player has won or if board is full. if not if calls getnat to ask for next move, 
-- if move is valid then it performs the move if not error and rerun run with same parameters
run' :: Board -> Player -> IO ()
run' board player 
	 | wins O board = putStrLn "Jugador O gana!\n"
         | wins X board = putStrLn "Jugador X gana!\n"
         | full board   = putStrLn "Empate!\n"
         | otherwise = 
            do position <- getNat ("Jugador " ++ show player ++ ", introduce tu movimiento: " ) -- Gets the position that the player wants.
               case move board position player of 
                  [] -> do putStrLn "Error: Movimiento no valido" -- empty list -> not valid.
                           run' board player -- ask again
                  [board'] -> run board' (next player) -- displays board and turn to the next player.
				  
humanvshuman :: IO ()
humanvshuman = run empty O