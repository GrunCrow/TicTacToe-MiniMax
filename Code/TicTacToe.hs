-- TicTacToe with AI algorithm (minimax)
--
-- references:
-- https://towardsdatascience.com/tic-tac-toe-creating-unbeatable-ai-with-minimax-algorithm-8af9e52c1e7d
--
-- TODO: compare with alpha-beta?

import Data.List
import Data.Char

main :: IO()
main :: do
	putStrLn $ take 10 $ repeat '\n'
	putStrLn "\n x o x o	Tic Tac Toe		x o x o\n"
	putStrLn "Columns = letters"
	putStrLn "Rows = Numbers\n"