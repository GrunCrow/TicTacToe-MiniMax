module TicTacToe.BoardFunctions where
import TicTacToe.Objects as Objects
import Data.List

-- ============================================================================
--				Board Functions
-- ============================================================================
-- empty: generates an empty Board
empty :: Board
empty = replicate Objects.size (replicate Objects.size B) -- replicate 3 (replicate 3 B)
-- empty -> [[B,B,B],[B,B,B],[B,B,B]]

-- full: check if the Board is full
full:: Board -> Bool
full  b = and (map (all (/= B)) b)
-- and [True,True,True]  -> True
-- and [True,False,True] -> False

-- True -> The board is full
-- False -> There is, at least, one blank in the board.


-- turn: returns the next player by examining the input board
-- we assume player O goes first
turn :: Board -> Player
turn board = if os <= xs then O else X	-- if number of Os > Xs then is O turn, else is X turn
		where
			os = length (filter (== O) ps) -- number of O's
			xs = length (filter (== X) ps) -- number of X's
			ps = concat board -- concat board merge the 2D list in 1D list.

-- diagonal: returns main diagonal of the Board 
diagonal :: Board -> [Player]
diagonal diag = [diag !! n !! n | n <- [0..size-1]] 
-- !! Operator gets the n-th element in the list. If we have 2D list, 
-- we use twice the operator because we want the diagonal items.

-- diagonal [[O,X,O],[X,B,O],[B,B,B]] -> [O,B,B]

-- wins: decides if a player has won a Board, callable by won that indicates what player has won
wins :: Player -> Board -> Bool
wins player board = row || cols || p_diag || s_diag
			where
				row = or (map (all (== player)) board)
				cols =  or (map (all (== player)) (transpose board) )
				p_diag = (all (== player)) (diagonal board)
				s_diag = (all (== player)) (diagonal (map reverse board))