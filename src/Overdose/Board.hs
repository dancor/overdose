module Overdose.Board where 

import qualified Data.Map as M
import Text.PrettyPrint
import Control.Monad.Trans
import System.Random
import Data.Maybe
-- Static Board 

data Dir = D | R | L  

type POS = (Int, Int)

data Connection = CL | CR | CT | CB

data Color  = Yellow | Red | Blue
              deriving Eq

instance Show Color where 
    show Yellow = "Y"
    show Red = "R"
    show Blue = "B"


showBoard board = vcat $ do 
  v <- [1..vertMax]
  return $ hcat [case M.lookup (h, v) board of 
                 Nothing -> text " "
                 Just p -> text $ show $ color p
                             | h <- [1..horizMax]]
       

initBoard = do
  board <- sequence [ do 
                      square <- assignSquare
                      case square of 
                           Nothing -> return Nothing
                           Just color -> 
                               return $ Just ((h,v), Piece {isVirus=True, connected = Nothing, color = color}) |
                      v <- [1..(vertMax - 4)],
                      h <- [1..horizMax]
             ]
  return $ M.fromList $ catMaybes board
      where assignSquare = do 
                flip <- randomIO
                if (flip::Double) < 0.7 then 
                    return Nothing 
                 else if flip > 0.9 then  return $ Just Red 
                 else if flip > 0.8 then return $ Just Yellow
                 else if flip > 0.7 then return $ Just Blue 
                 else return Nothing

data Piece = Piece {
      connected :: Maybe Connection,
      color :: Color,
      isVirus :: Bool
}

type Board = M.Map POS Piece

vertMax = 16
horizMax = 8

allPos = [(x,y) | y <- [1 .. vertMax], x <- [1 .. horizMax]]




delPos D (x, y) =  (x,   y-1) 
delPos L (x, y) =  (x-1, y) 
delPos R (x, y) =  (x+1, y) 


xpos (x,_)= x
ypos (_,y)= y

hasPiece :: Board -> POS -> Bool
hasPiece board pos = 
    case M.lookup pos board of 
      Nothing -> False 
      (Just _) -> True

isBlocked dir board pos = 
    (hasPiece board $ delPos dir pos)  ||
    (ypos pos == 0) ||  
    (xpos pos == 0) || 
    (xpos pos == (vertMax + 1))

addPiece board curPiece = 
    M.insert (otherPos curPiece) otherPiece $ M.insert (bottomLeft curPiece) blPiece board 
        where           
          blPiece = Piece {
                      isVirus = False,
                          connected = case orient curPiece of
                                        Flat -> Just CR
                                        Up -> Just CT,
                          color = case order curPiece of 
                                    Reg -> fst $ colors curPiece
                                    Flip -> snd $ colors curPiece
                    } 
          otherPiece = Piece {
                         isVirus = False,
                          connected = case orient curPiece of
                                        Flat -> Just CL
                                        Up -> Just CB,
                          color = case order curPiece of 
                                    Reg -> snd $ colors curPiece
                                    Flip -> fst $ colors curPiece
                    } 

findMatches board = do 
    pos <- allPos 
    dir <- [D,R]
    color <- [Red, Yellow, Blue]
    (checkMatches dir color pos board) 


matchColor pos colorcheck board = 
    case M.lookup pos board of 
      Nothing -> False 
      (Just spot ) -> (color spot) == colorcheck

checkMatches dir color pos board = cm color pos 0 []
    where 
      cm color pos len accum =
          if matchColor pos color board then 
              cm color (delPos dir pos) (len + 1) (pos:accum) 
          else 
              (if len >=4 then accum else []) 



-- user dropping mode 

data Orientation = Flat | Up

data ColorOrder  = Reg | Flip

data CurPiece = CurPiece {
      bottomLeft :: POS,
      colors :: (Color, Color),
      orient :: Orientation,
      order :: ColorOrder
}

rotatePiece curPiece = 
    case (orient curPiece, order curPiece) of 
      (Flat, Reg) -> curPiece{orient = Up} 
      (Up, Reg) -> curPiece{orient= Flat, order = Flip} 
      (Flat, Flip) -> curPiece{orient = Up} 
      (Up, Flip) -> curPiece{orient = Flat, order = Reg} 

otherPos curPiece = 
    case orient curPiece of 
       Flat -> (x+1, y)
       Up -> (x, y+1)
    where  (x,y) = bottomLeft curPiece 
getPiecePos curPiece = 
    [bottomLeft curPiece, otherPos curPiece] 

-- getBottoms curPiece = 
--     [(x,y)] ++ (case  orient curPiece of 
--                   Flat -> [(x+1,y)]
--                   Up -> [])
--     where (x,y) = bottomLeft curPiece

movePiece dir board curPiece =
    if canMovePiece dir board curPiece then 
        curPiece { bottomLeft = delPos dir $ bottomLeft curPiece } 
    else curPiece

canMovePiece dir board curPiece = 
    any (isBlocked dir board) movedPos
     where movedPos = map (delPos dir ) poses
           poses = getPiecePos curPiece

-- uncontrolled falling mode 


-- Tests

