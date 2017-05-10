module Main where
import MyUtil
import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Concurrent

type Board = Array (Int,Int) Char

main = loop $ initBoard grider

loop :: Board -> IO ()
loop b = do
  printBoard b
  s <- getLine
  if s == "q"
    then return ()
    else loop $ updateBoard b

_x = 20
_y = 20
alive = '*'
dead = '.'
blinker = [(5,5),(5,6),(5,7)] :: [(Int,Int)]
grider = [(5,5),(5,6),(5,7),(6,5),(7,6)] :: [(Int,Int)]

board :: Board
board = array ((1,1),(_x,_y)) [((x, y), dead) | x <- [1.._x], y <- [1.._y]]

initBoard :: [(Int,Int)] -> Board
initBoard ps = runSTArray $ do
  a <- thaw board
  forM_ ps $ \p ->
    writeArray a p alive
  return a

updateBoard :: Board -> Board
updateBoard b = runSTArray $ do
  a <- thaw b
  forM_ [1.._y] $ \y ->
    forM_ [1.._x] $ \x ->
      writeArray a (x,y) $ nextCell b (x,y)
  return a

printBoard :: Board -> IO()
printBoard board =  putStrLn $ unlines [[(board ! (x, y)) | y <- [1.._y]] | x <- [1.._x]]

nextCell :: Board -> (Int,Int) -> Char
nextCell b p = if isAlive b p
  then case aliveNeighborCells b p of
    2 -> alive
    3 -> alive
    _ -> dead
  else case aliveNeighborCells b p of
    3 -> alive
    _ -> dead

isAlive :: Board -> (Int,Int) -> Bool
isAlive b p = if b ! p == alive then True else False

aliveNeighborCells :: Board -> (Int,Int) -> Int
aliveNeighborCells b p = count alive $ neighborCellList b p

neighborCellList :: Board -> (Int,Int) -> [Char]
neighborCellList b (x,y) = [b ! (around (x + i,y + j)) |
                            i <- [-1..1], j <- [-1..1],
                            (i,j) /= (0,0)
                            -- x + i >= 1, x + i <= _x, y + j >= 1, y + j <= _y
                                                       ]
around :: (Int,Int) -> (Int,Int)
around (x,y) = (around' x, around' y)
  where around' :: Int -> Int
        around' n
          | n <= 0 = 20
          | n >= 21 = 1
          | otherwise = n

