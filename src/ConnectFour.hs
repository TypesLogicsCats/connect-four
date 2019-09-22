module ConnectFour where

import Data.List (concat, intersperse)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data Error = ColumnIsFull | InvalidIndex

instance Show Error where
    show ColumnIsFull = "Column is full."
    show InvalidIndex = "Invalid index."

data Piece = Black | Red
    deriving (Eq)

instance Show Piece where
    show Black = "B"
    show Red = "R"

negatePiece :: Piece -> Piece
negatePiece Black = Red
negatePiece Red = Black

data Players a = Pair { red :: a, black :: a }

instance Functor Players where
    fmap f (Pair red black) = Pair (f red) (f black)

data Column = Col { pieces :: [Piece], height :: Int }

instance Show Column where
    show (Col pieces _) =
        concat $ intersperse " | " $ fmap show $ reverse pieces

emptyCol :: Column
emptyCol = Col [] 0

dropPieceInCol :: Piece -> Column -> Either Error Column
dropPieceInCol piece (Col pieces height) =
    if height > 7 then
        Left ColumnIsFull
    else
        Right $ Col (piece:pieces) (height + 1)

newtype Board = Board { columns :: [Column] }

instance Show Board where
  -- Show the board sideways because it's a list of columns, and transposing it
  -- would be inconvenient and needlessly inefficient just for display purposes
  show (Board columns) = go 1 columns
    where
      go colNum (col:cols) =
          show colNum ++ " | " ++ show col ++ "\n" ++ go (colNum + 1) cols
      go _ [] = ""

emptyBoard :: Board
emptyBoard = Board $ replicate 8 emptyCol

dropPieceInCols :: Piece -> Int -> [Column] -> Either Error [Column]
dropPieceInCols _ _ [] = Left InvalidIndex
dropPieceInCols piece 0 (col:cols) = do
    col <- dropPieceInCol piece col
    return $ col:cols
dropPieceInCols piece n (col:cols) = do
    cols <- dropPieceInCols piece (n - 1) cols
    return $ col:cols

dropPiece :: Piece -> Int -> Board -> Either Error Board
dropPiece piece idx (Board cols) = fmap Board (dropPieceInCols piece idx cols)

longestCol :: Piece -> [Piece] -> (Int, [Piece])
longestCol _ [] = (0, [])
longestCol piece (piece':pieces)
    | piece == piece' =
        let (length, pieces) = longestCol piece pieces in
        (length + 1, pieces)
    | otherwise = (0, pieces)

scoreCol :: [Piece] -> Players [Int]
scoreCol [] = Pair [] []
scoreCol (piece:pieces) =
    let (length, pieces) = longestCol piece pieces in
    let Pair red black = scoreCol pieces in
    case piece of
        Red -> Pair (length:red) black
        Black -> Pair red (length:black)

--                                 (length, rest of current row)
longestRow :: Piece -> [Column] -> (Int   , [Column]           )

-- Empty board:
longestRow _ [] = (0, [])

-- Empty column:
--        |
--        | cols..
--        |
longestRow piece ((Col [] _):cols) = (0, cols)

-- Nonempty column:
--        |
-- ..     | cols..
-- piece' |
longestRow piece ((Col (piece':col) _):cols)
    | piece == piece' =
        let (length, restOfRow) = longestRow piece cols in
        (length + 1, restOfRow)
    | otherwise = (0, cols)

scoreRow :: [Column] -> Players [Int]
scoreRow [] = Pair [] []
scoreRow ((Col [] _):cols) = scoreRow cols
scoreRow ((Col (piece:col) _):cols) =
    let (length, restOfRow) = longestRow piece cols in
    let Pair red black = scoreRow restOfRow in
    case piece of
        Red -> Pair (length:red) black
        Black -> Pair red (length:black)

dropRow :: [Column] -> [Column]
dropRow [] = []
dropRow ((Col [] size):xss) = Col [] size : dropRow xss
-- We don't use the size where this function is used, but decrement it anyway
dropRow ((Col (_:xs) size):xss) = (Col xs (size - 1)) : dropRow xss

isAllEmpty [] = True
isAllEmpty ((Col [] _):xs) = isAllEmpty xs
isAllEmpty ((Col (_:_) _):xs) = False

scoreRows :: [Column] -> [Players [Int]]
scoreRows board
    | isAllEmpty board = []
    | otherwise = scoreRow board : scoreRows (dropRow board)

-- _ | _ | etc..
-- _ | R | _
-- R | _ | _
longestBotLeftToTopRight :: Piece -> [Column] -> (Int, [Column])
longestBotLeftToTopRight _ [] = (0, [])
longestBotLeftToTopRight _ ((Col [] _):cols) = (0, dropRow cols)
longestBotLeftToTopRight piece ((Col (piece':_) _):cols)
    | piece == piece' =
      let (length, cols'') = longestBotLeftToTopRight piece cols' in
      (length + 1, cols'')
    | otherwise = (0, cols')
  where cols' = dropRow cols

scoreBotLeftToTopRight :: [Column] -> Players [Int]
scoreBotLeftToTopRight [] = Pair [] []
scoreBotLeftToTopRight ((Col [] _):cols) = scoreBotLeftToTopRight cols
scoreBotLeftToTopRight ((Col (piece:_) _):cols) =
    let (length, restOfBoard) = longestBotLeftToTopRight piece $ dropRow cols in
    let Pair red black = scoreBotLeftToTopRight restOfBoard in
    case piece of
        Red -> Pair (length:red) black
        Black -> Pair red (length:black)

scoreBotRightToTopLeft :: [Column] -> Players [Int]
scoreBotRightToTopLeft = scoreBotLeftToTopRight . reverse

data Score = Fin Int | Inf | NegInf

instance Semigroup Score where
    Fin m  <> Fin n = Fin (m + n)
    Inf    <> _        = Inf
    NegInf <> _        = NegInf
    _      <> Inf      = Inf
    _      <> NegInf   = NegInf

instance Monoid Score where
    mempty = Fin 0

convertRawScores :: [Int] -> Score
convertRawScores [] = Fin 0
convertRawScores (x:xs)
    | x >= 3 = Inf
    | otherwise = Fin x <> convertRawScores xs

negateScore :: Score -> Score
negateScore (Fin n) = Fin (-n)
negateScore Inf = NegInf
negateScore NegInf = Inf

data Tree = Tree
    { board :: Board -- ^ The board of the current game state
    , moves :: [Either Error Tree]
      -- ^ The next game states; this is an infinite list and the out-of-bound
      --   elements map to @Left _@.
    }

gameTree :: Piece -> Board -> Tree
gameTree piece board = Tree board $ map f [0..]
  where f idx = fmap (gameTree $ negatePiece piece) $ dropPiece piece idx board

initialTree = gameTree Red emptyBoard

readMove :: Tree -> IO Tree
readMove tree = do
    putStr "Your move: "
    hFlush stdout
    s <- getLine
    case readMaybe s of
        Nothing -> do
            putStrLn "Not a number."
            readMove tree
        Just idx -> case (moves tree) !! (idx - 1) of
            Left e -> do
                print e
                readMove tree
            Right tree -> return tree

play :: Tree -> IO ()
play tree = do
    tree <- readMove tree
    putStr $ show $ board tree
    play tree
