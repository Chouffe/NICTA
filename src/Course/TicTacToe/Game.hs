{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.TicTacToe.Game where


import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.Interactive
import           Course.List
import           Course.Monad
import           Course.Optional
import           Course.State
import           Course.StateT
import           Course.Traversable


-- TODO: move certain functions to utils

data Player = P1 | P2 deriving (Eq, Show)

type Position = (Integer, Integer)

data Cell = CP1 | CP2 | E deriving (Eq)

instance Show Cell where
  show CP1 = "x"
  show CP2 = "0"
  show E   = " "

data Board =
    Board
    ( (Cell, Cell, Cell)
    , (Cell, Cell, Cell)
    , (Cell, Cell, Cell)
    )
  | FinishedBoard
  ( (Cell, Cell, Cell)
  , (Cell, Cell, Cell)
  , (Cell, Cell, Cell)
  )
  deriving (Eq, Show)

-- TODO: instance Show Board instead

showRow :: (Cell, Cell, Cell) -> Chars
showRow (x, y, z) =
  show' x ++ "|" ++ show' y ++ "|" ++ show' z

showBoard :: Board -> Chars
showBoard b@(FinishedBoard _) = showBoard b
showBoard (Board (row1, row2, row3)) =
  showRow row1 ++ "\n" ++ showRow row2 ++ "\n" ++ showRow row3 ++ "\n"

tripleToList :: (a, a, a) -> List a
tripleToList (x, y, z) = x:.y:.z:.Nil

position :: Integer -> Integer -> Optional Position
position x y =
  if isValidPosition x y
  then Full (x, y)
  else Empty

isValidPosition :: Integer -> Integer -> Bool
isValidPosition x y = x >= 0 && x < 3 && y >= 0 && y < 3

emptyBoard :: Board
emptyBoard =
  Board
  ( (E, E, E)
  , (E, E, E)
  , (E, E, E))

third :: (a, b, c) -> c
third (_, _, c) = c

-- TODO: use lenses instead of pattern matching
cellValue :: Board -> Position -> Optional Cell
cellValue (FinishedBoard board) pos = cellValue (Board board) pos
cellValue (Board (row1, row2, row3)) (x, y) =
  let (x00, x01, x02) = row1
      (x10, x11, x12) = row2
      (x20, x21, x22) = row3
  in case (x, y) of
       (0, 0) -> Full x00
       (0, 1) -> Full x01
       (0, 2) -> Full x02
       (1, 0) -> Full x10
       (1, 1) -> Full x11
       (1, 2) -> Full x12
       (2, 0) -> Full x20
       (2, 1) -> Full x21
       (2, 2) -> Full x22
       _      -> Empty

positionIsOccupied :: Board -> Position -> Bool
positionIsOccupied board pos =
  case cellValue board pos of
    Empty  -> False
    Full E -> False
    _      -> True

updaceCellWith :: Position -> Board -> (Cell -> Cell) -> Optional Board
updaceCellWith _ (FinishedBoard _) _ = Empty
updaceCellWith (x, y) (Board (row1, row2, row3)) f =
  let (x00, x01, x02) = row1
      (x10, x11, x12) = row2
      (x20, x21, x22) = row3
  in case (x, y) of
       (0, 0) -> Full $ Board ((f x00, x01, x02), row2, row3)
       (0, 1) -> Full $ Board ((x00, f x01, x02), row2, row3)
       (0, 2) -> Full $ Board ((x00, x01, f x02), row2, row3)
       (1, 0) -> Full $ Board (row1, (f x10, x11, x12), row3)
       (1, 1) -> Full $ Board (row1, (x10, f x11, x12), row3)
       (1, 2) -> Full $ Board (row1, (x10, x11, f x12), row3)
       (2, 0) -> Full $ Board (row1, row2, (f x20, x21, x22))
       (2, 1) -> Full $ Board (row1, row2, (x20, f x21, x22))
       (2, 2) -> Full $ Board (row1, row2, (x20, x21, f x22))
       _      -> Empty

playerToCell :: Player -> Cell
playerToCell P1 = CP1
playerToCell P2 = CP2

cellToPlayer :: Cell -> Optional Player
cellToPlayer E   = Empty
cellToPlayer CP1 = Full P1
cellToPlayer CP2 = Full P2

updateCellWithPlayer :: Position -> Board -> Player -> Optional Board
updateCellWithPlayer pos board player =
  updaceCellWith pos board (const (playerToCell player))

move :: Position -> Player -> Board -> Optional Board
move _ _ (FinishedBoard b) = Full $ FinishedBoard b
move (x, y) player board =
  if not (isValidPosition x y)
  then Empty
  else
    case cellValue board (x, y) of
      Full E -> updateCellWithPlayer (x, y) board player
      _      -> Empty

data Winner = W1 | W2 | Draw
  deriving (Eq, Show)

column1 :: Board -> (Cell, Cell, Cell)
column1 (FinishedBoard b)                               = column1 (Board b)
column1 (Board ((x00, _, _), (x10, _, _), (x20, _, _))) = (x00, x10, x20)

column2 :: Board -> (Cell, Cell, Cell)
column2 (FinishedBoard b)                               = column2 (Board b)
column2 (Board ((_, x01, _), (_, x11, _), (_, x21, _))) = (x01, x11, x21)

column3 :: Board -> (Cell, Cell, Cell)
column3 (FinishedBoard b)                               = column3 (Board b)
column3 (Board ((_, _, x02), (_, _, x12), (_, _, x22))) = (x02, x12, x22)

-- TODO diagonales
diag1:: Board -> (Cell, Cell, Cell)
diag1 (FinishedBoard b)                               =  diag1 (Board b)
diag1 (Board ((x00, _, _), (_, x11, _), (_, _, x22))) = (x00, x11, x22)

diag2:: Board -> (Cell, Cell, Cell)
diag2 (FinishedBoard b)                               =  diag2 (Board b)
diag2 (Board ((_, _, x02), (_, x11, _), (x20, _, _))) = (x02, x11, x20)

playerToWinner :: Player -> Winner
playerToWinner P1 = W1
playerToWinner P2 = W2

cellToWinner :: Cell -> Optional Winner
cellToWinner CP1 = Full W1
cellToWinner CP2 = Full W2
cellToWinner E   = Empty

fstTriple :: (a, b ,c) -> a
fstTriple (a, _, _) = a

whoWon :: Board -> Optional Winner
whoWon (Board _) = Empty
whoWon board     = winner board

numberEmptyCell :: Board -> Int
numberEmptyCell (FinishedBoard b) = numberEmptyCell (Board b)
numberEmptyCell (Board (row1, row2, row3)) =
  length $ filter (==E) $ tripleToList row1 ++ tripleToList row2 ++ tripleToList row3

isDraw :: Board -> Optional Bool
isDraw (Board _)               = Empty
isDraw board@(FinishedBoard _) = Full $ winner board == Full Draw

isWinner :: Board -> Bool
isWinner board =
  case winner board of
    Empty     -> False
    Full Draw -> False
    _         -> True

winner :: Board -> Optional Winner
winner (FinishedBoard b) = winner (Board b)
winner b@(Board (row1, row2, row3))

  -- Row checks
  | tripleEq row1 = cellToWinner $ fstTriple row1
  | tripleEq row2 = cellToWinner $ fstTriple row2
  | tripleEq row3 = cellToWinner $ fstTriple row3

  -- Column checks
  | tripleEq (column1 b) = cellToWinner $ fstTriple (column1 b)
  | tripleEq (column2 b) = cellToWinner $ fstTriple (column2 b)
  | tripleEq (column3 b) = cellToWinner $ fstTriple (column3 b)

  -- Diagonal checks
  | tripleEq (diag1 b) = cellToWinner $ fstTriple (diag1 b)
  | tripleEq (diag2 b) = cellToWinner $ fstTriple (diag2 b)

  -- Available cells
  | numberEmptyCell b > 0 = Empty

  -- Draw
  | otherwise = Full Draw

  where tripleEq (x, y, z) = x == y && y == z && x /= E

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- Test some moves
game :: Optional Board
game = move (1, 1) P1 emptyBoard >>= move (0, 0) P2 >>= move (2, 1) P1

-- Game can be run by using `gameLoop`

gameRound :: Board -> Player -> IO Board
gameRound board player = do
  putStrLn $ "Player to play: " ++ show' player
  putStrLn "Board"
  putStrLn $ showBoard board
  putStr "Enter a x: "
  x <- toInteger . digitToInt <$> getChar
  putStrLn ""
  putStr "Enter a y: "
  y <- toInteger . digitToInt <$> getChar
  putStrLn ""
  let pos = (x, y)
  if positionIsOccupied board pos
    then putStrLn "Please pick a free position..." >> gameRound board player
    else
      case move pos player board of
        Empty -> putStrLn "Could not move there..." >> gameRound board player
        Full newBoard -> pure newBoard


endGame :: Board -> IO Board
endGame board@(Board _) = putStrLn "Unfinished Game..." >> pure board
endGame board@(FinishedBoard _) = do
  let w = winner board
  putStrLn "The winner is: "
  putStrLn $ show' w
  pure board

gameLoop :: Board -> Player -> Integer -> IO Board
gameLoop board@(FinishedBoard _) _ _ = putStrLn "Game Over" >> pure board
gameLoop board@(Board b) player k =
  if isWinner board
  then endGame (FinishedBoard b)
  else do
    putStrLn $ "Round number: " ++ show' k
    newBoard <- gameRound board player
    gameLoop newBoard (nextPlayer player) (k + 1)

-- TODO: create a List version of TicTacToe to avoid so many pattern matchings
-- TODO: create a State version of this and use until
-- TODO: test game with quickCheck and Hspec

type GameState = (Board, Integer, Player)

-- (s -> IO (value, state))
-- liftIO

-- gameRound2 :: StateT GameState IO ()
-- gameRound2 = do
--   (board, k, player) <- getT
--   liftIO $ putStrLn $ "Player to play: " ++ show' player
--   liftIO $ putStrLn "Board"
--   liftIO $ putStrLn $ showBoard board
--   liftIO $ putStr "Enter a x: "
--   x <- liftIO $ toInteger . digitToInt <$> getChar
--   liftIO $ putStrLn ""
--   liftIO $ putStr "Enter a y: "
--   y <- liftIO $ toInteger . digitToInt <$> getChar
--   liftIO $ putStrLn ""
--   let pos = (x, y)
--   if positionIsOccupied board pos
--     then liftIO (putStrLn "Pick a free position...") >> gameRound2
--     else
--       case move pos player board of
--         Empty         -> liftIO (putStrLn "Pick a free position...") >> gameRound2
--         Full newBoard -> modify (\(_, n, player) -> (newBoard, n, player))
