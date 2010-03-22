import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.UI.SDL as SDL
import Data.Monoid
import Overdose.Board
import System.Environment(getArgs)
import Control.Monad 
import Data.Maybe
import Debug.Trace

resX, resY :: Int
resX = 480
resY = 480

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()


quadrants :: (Monoid a) => Draw.Image a -> Draw.Image a
quadrants img = mconcat [
    (Draw.translate (-0.5,0.5) %%),
    (Draw.translate (0.5,0.5)   `Draw.compose` Draw.rotate (-pi/2) %%),
    (Draw.translate (0.5,-0.5)  `Draw.compose` Draw.rotate pi %%),
    (Draw.translate (-0.5,-0.5) `Draw.compose` Draw.rotate (pi/2) %%)] (Draw.scale 0.5 0.5 %% img)


square = (Draw.scale 0.66 0.66 `Draw.compose`  Draw.rotate (pi/4)) %% Draw.regularPoly 4 

toColor Blue = Draw.Color 0 0 1 1
toColor Red = Draw.Color 1 0 0 1
toColor Yellow = Draw.Color 0 1 0 1


drawBoard game = (Draw.scale 0.1 0.1 `Draw.compose` Draw.translate (-4.0, -8.0) %%) $ mconcat $  do 
  (pos, squareData) <- getAllPos game 
  guard $ isJust squareData
  
  let mycolor = toColor $ color $ fromJust squareData 
  let mysquare = Draw.tint mycolor square
  let finalsquare = if isVirus $ fromJust squareData then
                        ((Draw.scale  0.3 0.3) %% square) `mappend` mysquare
                    else mysquare 
  return $ Draw.translate (fromIntegral $ xpos pos, fromIntegral $ ypos pos) %% finalsquare


main :: IO ()
main = do
    initScreen
    game <- initGame
    Draw.clearRender $ drawBoard game
    SDL.glSwapBuffers
    waitClose game
    SDL.quit
    return ()
    where

    waitClose game = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             SDL.KeyDown key -> do 
                game1 <- case SDL.symKey key of
                           SDL.SDLK_UP ->
                               return $ rotate game 
                           SDL.SDLK_LEFT ->
                               return $ trace "left" $ move L game
                           SDL.SDLK_RIGHT ->
                               return $ trace "right" $ move R game
                           _ -> 
                               advanceGame game
                  
                Draw.clearRender $ drawBoard game1
                SDL.glSwapBuffers
                waitClose game1
             _ -> waitClose game
