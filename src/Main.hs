import Overdose.Board
import System.Posix
import Text.PrettyPrint
import System.Console.ANSI

main :: IO ()
main = do
  game <- initGame
  run 1000 game 
      where  
        run 0 game = return ()
        run n game = do
          sleep 1
          clearScreen
          putStrLn $ render $ showGame game
          game' <- advanceGame game
          run (n-1) game'