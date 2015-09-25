module TicTacToe
where
import Data.Char

type BoardField = (Int, Int, Char)
type Board = [BoardField]

message :: String

--l
---d
----1:v
----1:x
----1:x
----i1e
----1:y
----i1e
---e
---d
----1:v
----1:o
----1:x
----i0e
----1:y
----i2e
---e
---d
----1:v
----1:x
----1:x
----i2e
----1:y
----i2e
---e
---d
----1:v
----1:o
----1:x
----i0e
----1:y
----i1e
---e
--e

message = "ld1:v1:x1:xi1e1:yi1eed1:v1:o1:xi0e1:yi2eed1:v1:x1:xi2e1:yi2eed1:v1:o1:xi0e1:yi1eee"

move :: String -> Maybe BoardField
move boardBencode = Just $ putXInEmptyField $ parseBoard boardBencode

parseBoard :: String -> Board
parseBoard ('l' : list) = parseBoardFields list
parseBoard _ = []

parseBoardFields :: String -> Board
parseBoardFields ('d' : dict) = 
    let
    (field, valueRest) = parseBoardField $ drop 2 dict
    in [field]
parseBoardFields _ = []

parseBoardField :: String -> (BoardField, String)
parseBoardField (key : value) = ((1, 1, key), value)

putXInEmptyField :: Board -> BoardField
putXInEmptyField board = head board
