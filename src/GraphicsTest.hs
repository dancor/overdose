import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.UI.SDL as SDL
import Data.Monoid
import Overdose.Board
import System.Environment(getArgs)
import Control.Monad 
import Data.Maybe
import Debug.Trace
import Control.Concurrent 
import Control.Concurrent.MVar 

resX, resY :: Int
resX = 480
resY = 480

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()


square = (Draw.scale 0.66 0.66 `Draw.compose`  Draw.rotate (pi/4)) %% Draw.regularPoly 4 
circle = (Draw.scale 0.5 0.5) %% Draw.circle
pill = Draw.scale 0.66 0.66 %%   
       ((Draw.scale 0.7 0.7 %% Draw.circle) `mappend` ((Draw.translate (0.5,0.0) `Draw.compose` Draw.scale 0.5 1.0 `Draw.compose` Draw.rotate (pi/4)) %% Draw.regularPoly 4) )
toColor Blue = Draw.Color 0 0 1 1
toColor Red = Draw.Color 1 0 0 1
toColor Yellow = Draw.Color 0 1 0 1


drawBoard game = (Draw.scale 0.1 0.1 `Draw.compose` Draw.translate (-4.0, -8.0) %%) $ mconcat $  do 
  (pos, squareData) <- getAllPos game 
  guard $ isJust squareData
  
  let mycolor = toColor $ color $ fromJust squareData 
  let startsquare = case connected $ fromJust squareData of 
                      Nothing -> circle
                      Just dir ->
                          case dir of 
                            U -> Draw.rotate (pi/2) %% pill 
                            L -> Draw.rotate (pi) %% pill 
                            D -> Draw.rotate (3*pi/2) %% pill 
                            R -> pill 
                            
  let mysquare = Draw.tint mycolor startsquare
  let finalsquare = if isVirus $ fromJust squareData then
                          ((Draw.scale  0.3 0.3) %% square) `mappend` mysquare  
                    else mysquare

  return $ Draw.translate (fromIntegral $ xpos pos, fromIntegral $ ypos pos) %% finalsquare



main :: IO ()
main = do
    initScreen
    startGame <- initGame
    gameIO <- newMVar startGame
    redraw gameIO
    forkIO $ forever (do 
                       game <- readMVar gameIO
                       threadDelay (case gameState game of 
                                      Falling -> 200000
                                      _ -> 1000000
                                      )
                       modifyMVar_ gameIO advanceGame
                       redraw gameIO
                     )
    waitClose gameIO
    SDL.quit
    return ()
    where
    redraw gameIO = do
         game <- readMVar gameIO
         Draw.clearRender $ drawBoard game
         SDL.glSwapBuffers

    waitClose gameIO = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             SDL.KeyDown key -> do 
                modifyMVar_ gameIO (\game -> case SDL.symKey key of
                                         SDL.SDLK_UP ->
                                             return $ rotate game 
                                         SDL.SDLK_LEFT ->
                                             return $ move L game
                                         SDL.SDLK_RIGHT ->
                                             return $ move R game
                                         SDL.SDLK_DOWN -> 
                                             advanceGame game
                                         _ -> return game
                                   )

                redraw gameIO
                waitClose gameIO
             _ -> waitClose gameIO 
