
module TicTacToe.DisplayBoard where

import TicTacToe.GameRules
import TicTacToe.Objects as Objects
import TicTacToe.BoardFunctions as BF

import Data.Char
import Data.List
import System.IO

-- ============================================================================
--				Display Board
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

