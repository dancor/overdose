module Overdose.Board where 

import qualified Data.Map as M


-- Static Board 

data Dir = D | R | L  

type POS = (Int, Int)

data Connection = CL | CR | CT | CB

data Color  = Y | R | B

data Piece = Piece {
      connected :: Connection,
      color :: Color
}

type Board = M.Map POS Piece

delPos D (x, y) =  (x,   y-1) 
delPos L (x, y) =  (x-1, y) 
delPos R (x, y) =  (x+1, y) 


hasPiece :: Board -> POS -> Bool
hasPiece board pos = 
    case M.lookup pos board of 
      Nothing -> False 
      (Just _) -> True

isBlocked dir board pos = 
    hasPiece board $ delPos dir pos  

-- user dropping mode 

data Orientation = Flat | Up

data ColorOrder  = Reg | Flip

data CurPiece = CurPiece {
      bottomLeft :: POS,
      colors :: (Color, Color),
      orient :: Orientation,
      order :: ColorOrder ,
}

getPiecePos curPiece = 
    [bottomLeft curPiece] ++ (case orient curPiece of 
                                Flat -> [(x+1, y)]
                                Up -> [(x, y+1)])

getBottoms curPiece = 
    [(x,y)] ++ (case  orient of 
                  Flat -> [(x+1,y)]
                  Up -> [])
    where (x,y) = bottomLeft curPiece

hasHit board curPiece = 
    any atRead $ getBottoms curPiece


movePiece dir board curPiece =
    if canMove then 
        curPiece { bottomLeft = delPos dir $ bottomLeft curPiece } 
    else curPiece
    where canMove = any (isBlocked board) movedPos
          movedPos = map (delPos dir) curPiece 
          poses = getPiecePos curPiece


-- uncontrolled falling mode 