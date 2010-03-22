module Overdose.Board where 

import qualified Data.Map as M


-- Static Board 

data Dir = D | R | L  

type POS = (Int, Int)

data Connection = CL | CR | CT | CB

data Color  = Yellow | Red | Blue

data Piece = Piece {
      connected :: Connection,
      color :: Color
}

type Board = M.Map POS Piece

vertMax = 16
horizMax = 8

allPos = [ (x,y) | y <- [1 .. vertMax], x <- [1 .. horizMax]]


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
                          connected = case orient curPiece of
                                        Flat -> CR
                                        Up -> CT,
                          color = case order curPiece of 
                                    Reg -> fst $ colors curPiece
                                    Flip -> snd $ colors curPiece
                    } 
          otherPiece = Piece {
                          connected = case orient curPiece of
                                        Flat -> CL
                                        Up -> CB,
                          color = case order curPiece of 
                                    Reg -> snd $ colors curPiece
                                    Flip -> fst $ colors curPiece
                    } 

-- findMatches = allPos 



-- checkMatch = 


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


