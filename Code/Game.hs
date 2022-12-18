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

import Data.List
import Data.Char

-- Board
-- rows = columns = 3
-- win = 3
-- depth = ?

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
