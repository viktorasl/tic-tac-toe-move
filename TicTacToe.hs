module TicTacToe
where

type BoardField = (Int, Int, Char)
type Board = [BoardField]

message :: String
message = "ld1:v1:x1:xi1e1:yi1eed1:v1:o1:xi0e1:yi2eed1:v1:x1:xi2e1:yi2eed1:v1:o1:xi0e1:yi1eee"

move :: String -> Maybe BoardField
move boardBencode = Just $ putXInEmptyField $ parseBoard boardBencode

parseBoard :: String -> Board
parseBoard _ = []

putXInEmptyField :: Board -> BoardField
putXInEmptyField _ = (0, 0, 'x')
