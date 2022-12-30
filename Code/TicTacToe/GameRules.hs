-- ============================================================================
--				Game Rules
-- ============================================================================
module TicTacToe.GameRules where
import TicTacToe.Objects
import Data.List
import Data.Char

--check if the move that the player wants to perfortm is valid
isValid :: Board -> Int -> Bool
isValid board position = 0 <= position && position < size^2 && concat board !! position == B

-- chop: breaks a list into maximal segments of a given length
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- move: empty list if no valid move or list with valid movements
move :: Board -> Int -> Player -> [Board]
move board position player =
	if isValid board position
		then [chop size (xs ++ [player] ++ ys)]
		else [] -- emptylist -> not valid.
	where (xs,B:ys) = splitAt position (concat board)
	
-- getNat: read position number that player wants to perform	
getNat :: String -> IO Int
getNat prompt = do 
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else
        do putStrLn "ERROR: Invalid number"
           getNat prompt
