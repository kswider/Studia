--Created by Krzysztof Świder
import Field
import Board
import Data.Maybe

main =
  do
  putStrLn "Podaj tryb gry:\n1 - gra dla dwoch graczy\n2 - gra przeciwko komputerowi"
  x <- getLine
  if (x == "1")
    then putStrLn (show emptyBoard) >> gameLoop emptyBoard Black
    else if (x == "2")
      then firstMove
      else main

gameLoop board@(Board fields) player =
  do
  putStrLn "Podaj kolumne (A-S): "
  x <- getLine
  putStrLn "Podaj rząd (1-19): "
  y <- getLine
  checkIfFieldIsEmpty (changeLetterToInt x) (read y) board 
  let newBoard = putFigure (changeLetterToInt x) (read y) board player
  putStrLn $ show newBoard
  if (checkWin newBoard player)
    then (putStrLn ("The winner is Player " ++ (show player)))
    else gameLoop newBoard (opposite player)

checkIfFieldIsEmpty x y board 
  |maybefield == Nothing = (putStrLn "Nieprawidlowe wspolrzędne, sprobuj ponownie!") >> (gameLoop2 board)
  |player /= Empty = (putStrLn "Pole jest zajete, spróbuj ponownie!") >> (gameLoop2 board )
  |otherwise = return ()
  where maybefield = takeField (x,y) board
        (Field player _) = fromJust maybefield
  
gameLoop2 board =
  do
  putStrLn "Podaj kolumne (A-S): "
  x <- getLine
  putStrLn "Podaj rząd (1-19): "
  y <- getLine
  checkIfFieldIsEmpty (changeLetterToInt x) (read y) board 
  let newBoard = putFigure (changeLetterToInt x) (read y) board Black
  putStrLn $ show newBoard
  if (checkWin newBoard Black)
    then (putStrLn ("The winner is Player X" ))
    else computerMove newBoard

firstMove =
  do
  let newBoard = putFigure 8 8 emptyBoard White
  putStrLn $ show newBoard
  gameLoop2 newBoard 

computerMove board =
  do
  let newBoard = chooseBestBoard board
  putStrLn $ show newBoard
  if (checkWin newBoard White)
    then (putStrLn ("The winner is Player O" ))
    else gameLoop2 newBoard
