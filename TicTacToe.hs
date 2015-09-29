module TicTacToe
where
import Data.Char
import Data.List

type BoardField = (Int, Int, Char)
type Board = [BoardField]

defaultField = (0, 0, 'o')

move :: String -> Maybe BoardField
move boardBencode = Just $ putXInEmptyField $ parseBoard boardBencode

putXInEmptyField :: Board -> BoardField
putXInEmptyField board = findEmptyField' board defaultField

-- Board parsing

parseBoard :: String -> Board
parseBoard ('l' : list) = parseBoardFields' list []

parseBoardFields' :: String -> Board -> Board
parseBoardFields' ('e' : dict) board = board
parseBoardFields' ('d' : dict) board =
    let
    (field, rest) = parseBoardField' dict defaultField
    in parseBoardFields' rest (field : board)

parseChar :: String -> Char
parseChar str = head $ drop 2 str

parseInt :: String -> Int
parseInt str = digitToInt $ head $ drop 1 str

assignValueToKey :: String -> Char -> BoardField -> (BoardField, String)
assignValueToKey str 'v' (x, y, v) = ((x, y, parseChar str), drop 3 str)
assignValueToKey str 'x' (x, y, v) = ((parseInt str, y, v), drop 3 str)
assignValueToKey str 'y' (x, y, v) = ((x, parseInt str, v), drop 3 str)

parseBoardField' :: String -> BoardField -> (BoardField, String)
parseBoardField' ('e' : rest) field = (field, rest) 
parseBoardField' str field =
    let
    (key, rest) = parseBoardFieldKey str
    (field', rest') = assignValueToKey rest key field
    in parseBoardField' rest' field'

parseBoardFieldKey :: String -> (Char, String)
parseBoardFieldKey str = (head $ drop 2 str, drop 3 str)

-- Empty field lookup

coordsEqual :: BoardField -> BoardField -> Bool
coordsEqual (x1, y1, _) (x2, y2, _) = if (x1 == x2 && y1 == y2) then True else False 

findEmptyField' :: Board -> BoardField -> BoardField
findEmptyField' board field = case indexOfField board field of
    Just _ -> findEmptyField' board $ nextField field
    Nothing -> field    

nextField :: BoardField -> BoardField
nextField (x, y, v) =
    let
        nextX = if (y == 2) then (x + 1) else x
        nextY = if (y == 2) then 0 else (y + 1)
    in (nextX, nextY, v)

indexOfField :: Board -> BoardField -> Maybe Int
indexOfField board field = findIndex (\field' -> coordsEqual field field') board
