module TicTacToe.HumanVSMachine where

import TicTacToe.Objects
import TicTacToe.DisplayBoard
import TicTacToe.BoardFunctions
import TicTacToe.GameRules

-- ============================================================================
--				GameTree
-- ============================================================================
gametree :: Board -> Player -> Tree Board
gametree board player = Node board [gametree board' (next player) | board' <- moves board player]

moves :: Board -> Player -> [Board]
moves board player
	| wins O board = []
	| wins X board = []
	| full board = []
	| otherwise = concat [move board position player | position <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]
	
depth :: Int
depth = 4

-- minimax: takes possible tree and returns another tree that have the winning player label 
minimax :: Tree Board -> Tree (Board, Player)
minimax (Node board [])
	| wins O board = Node (board, O) []
	| wins X board = Node (board, X) []
	| otherwise = Node (board, B) []
minimax (Node board ts)
	| turn board == O = Node (board, minimum ps) ts'
	| turn board == X = Node (board, maximum ps) ts'
						where
							ts' = map minimax ts
							ps = [p | Node (_,p) _ <- ts']
						
-- gives best move given a board and a player						
bestmoves :: Board -> Player -> Board
bestmoves board player = head [board' | Node (board', player') _ <- ts, player' == best]
							where
								tree = prune depth (gametree board player)
								Node (_,best) ts = minimax tree

-- inputs a player and returns a string to ask player to perform next move				
prompt :: Player -> String
prompt player = "Jugador " ++ show player ++ ", introduce tu movimiento: "							
-- ============================================================================
--				Run the Game
-- ============================================================================

play :: Board -> Player -> IO()
play board player = do
			putBoard board
			play' board player


							
play' :: Board -> Player -> IO ()
play' board player 
	| wins O board = putStrLn "Jugador O gana!\n"
        | wins X board = putStrLn "Jugador X gana!\n"
        | full board   = putStrLn "Empate!\n"
	| player == O = do 
			position <- getNat (prompt player)
			case move board position player of
				[] -> do play' board player	-- putStrLn "Error: Movimiento no valido"
				[board'] -> play board' (next player)
        | player == X = do (play $! (bestmoves board player)) (next player) -- putStrLn "Jugador X esta pensando..."
		
humanvsmachine :: IO ()
humanvsmachine = do play empty O		