module TicTacToe
where
import Data.Char

type BoardField = (Int, Int, Char)
type Board = [BoardField]

message :: String
message = "ld1:v1:x1:xi1e1:yi1eed1:v1:o1:xi0e1:yi2eed1:v1:x1:xi2e1:yi2eed1:v1:o1:xi0e1:yi1eee"

move :: String -> Maybe BoardField
move boardBencode = Just $ putXInEmptyField $ parseBoard boardBencode

parseBoard :: String -> Board
parseBoard ('d' : dict) = [(0, 0, 'd')]
parseBoard ('l' : list) = [(0, 0, 'l')]
parseBoard (l : string) = if (isNumber l) then [(0, 0, 's')] else []

putXInEmptyField :: Board -> BoardField
putXInEmptyField board = head board
