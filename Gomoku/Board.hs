--Created by Krzysztof Świder
module Board where

import Field
import Data.Maybe
import Data.List
import Data.Ord

newtype Board = Board {getValues::[Field]}

columnText = "   A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S\n"
instance Show Board where
  show (Board fieldList) = columnText ++ (concatMap show fieldList) ++ columnText
  
readBoard :: String -> Board
readBoard board = 
    let players = fmap readPlayer board
        coordinates = [(x,y) | y<-[1..19], x<-[1..19]]
    in Board (zipWith Field players coordinates)    

emptyBoard = Board [Field Empty (x, y)| y <-[1..19], x<-[1..19]]

fillField player (x,y) old@(Field thisplayer (thisx, thisy)) = if thisx == x && thisy == y && thisplayer == Empty then Field player (x,y) else old
putFigure x y (Board fields) player = Board (fmap (fillField player (x,y)) fields)

emptyFields :: Board -> [Field]
emptyFields (Board fields) = filter isEmpty fields

whiteFields (Board fields) = filter (isWhite) fields

type Coordinates = (Integer,Integer)
checkCoordinates :: Coordinates -> Field -> Bool
checkCoordinates (x,y) (Field _ (thisx,thisy)) = if x == thisx && y == thisy then True else False

takeField :: Coordinates -> Board -> Maybe Field
takeField (x,y) (Board fields) = find (checkCoordinates (x,y)) fields

takePlayer :: Field -> Player
takePlayer (Field player _) = player

checkField (x,y) player board
  |field /= Nothing && (takePlayer . fromJust) field == player = True
  |otherwise = False
   where field = takeField (x,y) board  
   
takeCoordinates :: Field -> Coordinates
takeCoordinates (Field _ (x,y)) = (x,y)

generateMoves :: Board -> [Board]
generateMoves board = fmap ( (\(x,y) -> putFigure x y board White) . takeCoordinates)  (emptyFields board)

type Vector = (Integer, Integer) --wektor służacy do sprawdzania pól w odpowiednim kierunku

countNumberOfFilled :: Board -> Player -> Vector -> Integer -> Field -> Integer
countNumberOfFilled board player vector@(x,y) left (Field thisplayer (thisx,thisy)) 
  |left == 0 = 0
  |field == Nothing && thisplayer == player = 1
  |field == Nothing = 0
  |thisplayer == player = 1 + countNumberOfFilled board player vector (left-1) (fromJust field)   
  |thisplayer == Empty = countNumberOfFilled board player vector (left-1) (fromJust field)  
  |otherwise = 0
   where field = takeField (thisx+x,thisy+y) board

   
data Threat = None | Double | Triple | Four | Five deriving (Eq, Show)

checkFieldAllDirections board player field
  |maxNumber == 5 = Five
  |maxNumber == 4 = Four
  |maxNumber == 3 = Triple
  |maxNumber == 2 = Double
  |otherwise = None
   where listWithSequences = [(countNumberOfFilled board player (1,0) 5 field),(countNumberOfFilled board player (1,1) 5 field),(countNumberOfFilled board player (0,1) 5 field),(countNumberOfFilled board player (-1,1) 5 field)]
         maxNumber = maximum listWithSequences

checkAllDirections player board@(Board fields) = (length filtered5) * 10000 + (length filtered4) * 100 + (length filtered3) * 10 + (length filtered2) * 1
   where listOfThreats = fmap (checkFieldAllDirections board player) (filter isWhite fields)
         filtered2 = filter (==Double) listOfThreats
         filtered3 = filter (==Triple) listOfThreats
         filtered4 = filter (==Four) listOfThreats
         filtered5 = filter (==Five) listOfThreats
  
check5 player board@(Board fields) --sprawdzam czy jest tylko jedna piątka,bo gdy będzie szostka to wykryje ją jako 2 piątki
  |points > 10000 && points < 20000 = True
  |otherwise = False
   where points = checkAllDirections player board
checkWin board player = check5 player board

evaluateBoard player board = (value,board)
  where value = checkAllDirections player board 
  
evaluateAllPossibleBoards player board = fmap (evaluateBoard player) (generateMoves board)

chooseBestBoard board = snd (maximumBy (comparing fst) boardlist)
  where boardlist = evaluateAllPossibleBoards White board
        