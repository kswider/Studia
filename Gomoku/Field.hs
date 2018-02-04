--Created by Krzysztof Świder
module Field where

data Player = White | Black | Empty deriving Eq
instance Show Player where
  show White = "O"
  show Black = "X"
  show Empty = " " 
  
readPlayer :: Char -> Player
readPlayer 'O' = White
readPlayer 'X' = Black
readPlayer ' ' = Empty
  
opposite player
  |player == White = Black
  |player == Black = White
  
data Field = Field {player::Player, coordinates::(Integer,Integer)} deriving Eq

instance Show Field where
  show (Field player (x,y))
    |x == 1 && y < 10 = " " ++ (show y) ++ "|" ++ (show player) ++ "|"
    |x == 1 = (show y) ++ "|" ++ (show player) ++ "|"
    |x == 19 = (show player) ++ "|" ++ (show y) ++ "\n"
    |otherwise = (show player) ++ "|"

--readField :: Char -> (Integer,Integer) -> Field
--readField p (x,y) = Field (readPlayer p) (x,y)

isEmpty:: Field -> Bool
isEmpty (Field player _) = if player == Empty then True else False 

--isNotEmpty:: Field -> Bool
isWhite (Field player _) = if player == White then True else False

changeLetterToInt letter
  |letter == "A" = 1
  |letter == "B" = 2
  |letter == "C" = 3
  |letter == "D" = 4
  |letter == "E" = 5
  |letter == "F" = 6
  |letter == "G" = 7
  |letter == "H" = 8
  |letter == "I" = 9
  |letter == "J" = 10
  |letter == "K" = 11
  |letter == "L" = 12
  |letter == "M" = 13
  |letter == "N" = 14
  |letter == "O" = 15
  |letter == "P" = 16
  |letter == "Q" = 17
  |letter == "R" = 18
  |letter == "S" = 19
  |otherwise = 0