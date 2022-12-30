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
module TicTacToe.Objects where
import Data.Char
import Data.List
import System.IO

-- ============================================================================
--				TicTacToe Objects
-- ============================================================================

-- o < blank < x
data Player = O | B | X deriving(Eq, Ord, Show) -- 0, blank or X are the possibilities that would be on the board
-- Board is componsed by 2d player array -> [[B,B,B],[B,B,B],[B,B,B]]
type Board = [[Player]]
data Tree a = Node a [Tree a] deriving Show
-- size of the grid (3x3)
size :: Int
size = 3

-- returns the next player
next :: Player -> Player
next O = X
next B = B
next X = O