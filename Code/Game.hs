-- TicTacToe with AI algorithm (minimax)
--
--	Alberto Fernández Merchán
--	Alba Márquez-Rodríguez
--
-- references:
-- https://towardsdatascience.com/tic-tac-toe-creating-unbeatable-ai-with-minimax-algorithm-8af9e52c1e7d
-- https://www.youtube.com/watch?v=IoCINPnt0us


-- https://www.youtube.com/watch?v=FYzDNC_rvF4


-- TODO ? graphics -> gloss library??
-- TODO: compare with alpha-beta?

-- 1. Create a basic version of TicTacToe Player vs Player
-- 2. Create a minimax algorithm so Player vs Computer
-- 3. Create a computer vs computer version?
-- 4. Create alpha-beta and make computer play vs itself one with minimax and the other win the alpha-beta

-- Board
-- rows = columns = 3
-- win = 3
-- depth = ?

import Data.List

-- ============================================================================
--										TicTacToe Objects
-- ============================================================================

-- o < blank < x
data Player = O | B | X deriving(Eq, Ord, Show) -- 0, blank or X are the possibilities that would be on the board

-- Board is componsed by 2d player array -> [[B,B,B],[B,B,B],[B,B,B]]
type Board = [[Player]]

-- size of the grid (3x3)
size :: Int
size = 3

-- returns to the next player
next :: Player -> Player
next O = X
next B = B
next X = O

-- ============================================================================
--										Board Functions
-- ============================================================================

-- empty: generates an empty Board
empty :: Board
empty = replicate size (replicate size B)
-- empty -> [[B,B,B],[B,B,B],[B,B,B]]

-- full: check if the Board is full
full:: Board -> Bool
full = all (/= B) . concat
-- full [[O,X,O],[X,B,O],[B,B,B]] -> false

-- turn: returns the next player by examining the input board
turn :: Board -> Player
turn g = if os <= xs then O else X	-- if number of Os > Xs then is O turn, else is X turn
		where
			os = length (filter (== O) ps)
			xs = length (filter (== X) ps)
			ps = concat g

-- diagonal: returns main diagonal of the Board 
diagonal :: Board -> [Player]
diagonal diag = [diag !! n !! n | n <- [0..size-1]]
-- diagonal [[O,X,O],[X,B,O],[B,B,B]] -> [O,B,B]

-- wins: decides if a player has won a Board, callable by won that indicates what player has won
wins :: Player -> Board -> Bool
wins player board = any line (rows ++ cols ++ diags)
			where
				line = all (==player)
				rows = board
				cols = transpose board
				diags = [diagonal board, diagonal (map reverse board)]

won :: Board -> Bool
won board = wins O board || wins X board

-- ============================================================================
--										Display Board
-- ============================================================================

-- tabulation: tabulates values between values of the list, for screen showing the board
tabulation :: a -> [a] -> [a]
tabulation x [] = []
tabulation x [y] = [y]
tabulation x (y:ys) = y : x : tabulation x ys

-- showPlayer: converts player value to alist of strings
showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer B = ["   ", "   ", "   "]

-- unlines: creates a string from a list of strings, it inserts new line characters between original strings
unlines_fun :: [String] -> String
unlines_fun [] = ""
unlines_fun (x:xs) = x ++ "\n" ++ unlines_fun xs

-- foldr1: apply the function to the items of the list from 2 to 2 starting from left
foldr_fun :: (a -> a -> a) -> [a] -> a
foldr_fun _ [x] = x
foldr_fun f (x:xs) = f x (foldr_fun f xs)
-- foldr_fun (+) [1,2,3] -> 6

showRow :: [Player] -> [String]
showRow = beside . tabulation bar . map showPlayer
			where
				beside = foldr_fun (zipWith (++))
				bar = replicate 3 "|"
-- showRow [B,X,O] -> ["   |   |   ","   | X | O ","   |   |   "]

-- displays the Board given 
putBoard :: Board -> IO ()
putBoard = putStrLn . unlines_fun . concat . tabulation bar . map showRow
			where bar = [replicate ((size*4)-1) '-']
-- putBoard [[O,X,O],[X,B,O],[B,B,B]]
--   |   |   
-- O | X | O 
--   |   |   
-------------
--   |   |   
-- X |   | O 
--   |   |   
-------------
--   |   |   
--   |   |   
--   |   |   