module Overdose.Board where 

import qualified Data.Map as M
import Text.PrettyPrint
import Control.Monad.Trans
import System.Random
import Data.Maybe
import Debug.Trace
import Text.Printf
-- Static Board 

data Dir = D | R | L | U
           deriving Eq
type POS = (Int, Int)

type Connection = Maybe Dir

data Color  = Yellow | Red | Blue
              deriving Eq

instance Show Color where 
    show Yellow = "Y"
    show Red = "R"
    show Blue = "B"

getAllPos game = do
  pos <- allPos
  return (pos, getPos pos board)
  where board = viewBoard game
    
getPos :: POS -> Board -> Maybe Piece
getPos pos board = 
     M.lookup pos board 

showBoard board = vcat $ do 
  v <- [vertMax, vertMax -1 .. 1]
  return $ (text $ printf "%2d" (v::Int) ) <+> hcat [case M.lookup (h, v) board of 
                 Nothing -> text " "
                 Just p -> text $ show $ color p
                             | h <- [horizMax, horizMax -1 .. 1]]
       
initGame :: IO Game
initGame = do 
  newPiece <- randPiece
  game <- initBoard
  return $ Game {
             gameState = User newPiece,
             staticBoard = game 
             
           }

initBoard :: IO Board
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
      connected :: Connection,
      color :: Color,
      isVirus :: Bool
}



type Board = M.Map POS Piece

vertMax = 16
horizMax = 8

allPos = [(x,y) | y <- [1 .. vertMax], x <- [1 .. horizMax]]




delPos D (x, y) =  (x,   y-1) 
delPos U (x, y) =  (x,   y+1) 
delPos L (x, y) =  (x-1, y) 
delPos R (x, y) =  (x+1, y) 


xpos (x,_)= x
ypos (_,y)= y

hasPiece :: Board -> POS -> Bool
hasPiece board pos = 
    case M.lookup pos board of 
      Nothing -> False 
      (Just _) ->  True

isBlocked board pos =  
    (hasPiece board pos)  ||
    ((ypos pos) < 1) ||  
    ((xpos pos) < 1) || 
    ((xpos pos) > (horizMax ))

addPiece :: Board -> CurPiece -> Board 
addPiece board curPiece = 
    M.insert (otherPos curPiece) otherPiece $ M.insert (bottomLeft curPiece) blPiece board 
        where           
          blPiece = Piece {
                      isVirus = False,
                          connected = case orient curPiece of
                                        Flat -> Just R
                                        Up -> Just U,
                          color = case order curPiece of 
                                    Reg -> fst $ colors curPiece
                                    Flip -> snd $ colors curPiece
                    } 
          otherPiece = Piece {
                         isVirus = False,
                          connected = case orient curPiece of
                                        Flat -> Just L
                                        Up -> Just D,
                          color = case order curPiece of 
                                    Reg -> snd $ colors curPiece
                                    Flip -> fst $ colors curPiece
                    } 

findMatches board = do 
    pos <- allPos 
    dir <- [D,R]
    color <- [Red, Yellow, Blue]
    (checkMatches dir color pos board) 

erase :: Board -> POS -> Board
erase board pos = 
    case getPos pos board of
      Nothing -> board
      Just piece ->
          case connected piece of 
            Nothing -> M.delete pos board 
            Just dir -> 
                M.adjust (\oldPiece -> oldPiece{connected = Nothing}) (delPos dir pos)  $ M.delete pos board 

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
                 deriving (Show)
data ColorOrder  = Reg | Flip
                   deriving (Show)
data CurPiece = CurPiece {
      bottomLeft :: POS,
      colors :: (Color, Color),
      orient :: Orientation,
      order :: ColorOrder
} deriving Show

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

move dir game = 
    case gameState game of 
      User curPiece -> 
          game {gameState = User $ fst $ movePiece dir (staticBoard game) $ curPiece} 
      _ -> game
rotate game = 
    case gameState game of 
      User curPiece -> 
          game {gameState = User $ rotatePiece curPiece} 
      _ -> game

movePiece dir board curPiece =
    if canMovePiece dir board curPiece then 
        let newPos = delPos dir $ bottomLeft curPiece in 
          (curPiece { bottomLeft = newPos  }, False) 
    else  (curPiece, True)

canMovePiece dir board curPiece = 
    not $ any (isBlocked board) movedPos
     where movedPos = map (delPos dir) poses
           poses = getPiecePos curPiece


randColor = do 
  r <- randomIO
  return $ if (r::Double) < 0.33 then
               Yellow 
           else if r < 0.66 then
               Red
           else 
               Blue

randPiece = do 
  a <- randColor 
  b <- randColor 
  return $ initPiece {colors = (a,b)} 
     

showGame game = 
    showBoard . viewBoard 

data GameState = User CurPiece | Falling

viewBoard game = 
    case gameState game of 
      User piece -> addPiece (staticBoard game) piece
      _ -> staticBoard game

data Game = Game {
      staticBoard :: Board,
      gameState :: GameState
    }

curPiece game = 
    case gameState game of 
      User piece -> piece 
      _ -> error "no current piece"

normalizeBoardStep board = 
    foldl normalizeRow (board,True) [1..vertMax]
  where 
    normalizeRow (board,b)  row  =
         foldl movePiece (board,b) drop 
        where drop = filter (shouldDropPiece board) [(col, row)  | col <- [1..horizMax] ] 
              movePiece (board, _) pos = 
                  (M.insert (delPos D pos) (fromJust piece) $ M.delete pos board, False) 
                  where piece = getPos pos board 

shouldDropPiece board pos  =
    case mpiece of 
      Nothing -> False
      Just piece -> let con = connected piece in
           if isVirus piece then
               False
           else if isBlocked board (delPos D pos)   then 
               False
           else if  con == Just R then
                (if isBlocked board (delPos D (delPos R pos)) then 
                     False
                 else True)
           else if con == Just L then
                (if isBlocked board (delPos D (delPos L pos)) then 
                     False
                 else True)             
           else
               True
    where mpiece = getPos pos board


advanceGame game = 
  case gameState game of 
    User curPiece -> 
        if stuck then
            do 
              let stuckBoard = addPiece board curPiece
              let (newBoard,_) = erasePieces stuckBoard
              return $ Game {staticBoard =newBoard,
                             gameState = Falling}
        else 
            return $ Game {staticBoard = board,
                           gameState = User newPiece
                           
                          }
        where (newPiece, stuck) = movePiece D board curPiece  

    Falling -> do
        let (newBoard, isDone) = normalizeBoardStep $ staticBoard game 
        if isDone then do 
            let (erasedBoard, isDone) = erasePieces newBoard
            if isDone then do 
                newPiece <- randPiece
                return $ Game {staticBoard =  erasedBoard,
                               gameState = User newPiece 
                              }
             else
                return $ Game {staticBoard = erasedBoard,
                               gameState = Falling
                              }
            
         else
            return $ Game {staticBoard = newBoard,
                           gameState = Falling
                          }
                        

  where               
      board = staticBoard game 
      
      erasePieces board = 
          (foldl erase board matches, null matches)
              where matches = findMatches board
-- uncontrolled falling mode 




-- Tests
initPiece = CurPiece {bottomLeft = (4,16),
                   orient = Flat,
                   order = Reg,
                   colors = (Red, Blue)
                  }


testPiece = CurPiece {bottomLeft = (4,16),
                   orient = Flat,
                   order = Reg,
                   colors = (Red, Blue)
                  }


