module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW (Window, Key(..), KeyState(..))
import qualified Graphics.UI.GLFW as GLFW

import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Control.Concurrent
import FRP.Elerea.Simple

import System.Console.GetOpt
import System.Environment
import System.Exit

type Pos = Vector2 GLdouble
type Speed = GLdouble
type PlayerSize = GLdouble
data Player = Player Pos PlayerSize Speed
type Movement = (Bool, Bool, Bool, Bool)

initialPlayer :: Player
initialPlayer = Player (Vector2 200 200) 20 10

data Options = Options
  { _optHelp :: Bool
  , _optWidth :: Int
  , _optHeight :: Int
  , _optTitle :: String
  }

defaultOptions :: Options
defaultOptions = Options
  { _optHelp = False
  , _optWidth = 640
  , _optHeight = 480
  , _optTitle = "Main"
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["help"] (NoArg (\opts -> opts { _optHelp = True }))
      "Print this message and quit."
  , Option "w" ["width"] (ReqArg (\o opts -> opts { _optWidth = read o}) "WIDTH")
      "Set the window width."
  , Option "h" ["height"] (ReqArg (\o opts -> opts { _optHeight = read o}) "HEIGHT")
      "Set the window height."
  , Option "t" ["title"] (ReqArg (\o opts -> opts { _optTitle = o }) "TITLE")
      "Set the window title."
  ]

parseOptions :: [String] -> IO (Options)
parseOptions argv =
  case getOpt Permute options argv of
    (o, _, []) -> return (foldl (flip id) defaultOptions o)
    (_, _, errs) -> helpMessage >>= \usage -> ioError (userError (concat errs ++ usage))

helpMessage :: IO (String)
helpMessage = do
  name <- getProgName
  let header = "Usage: " ++ name ++ " [Option...]"
  return $ usageInfo header options

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

main :: IO ()
main = do
  opts <- getArgs >>= parseOptions

  when (_optHelp opts) $ helpMessage >>= putStrLn >> exitSuccess

  let width = _optWidth opts
      height = _optHeight opts
      title = _optTitle opts

  (dirKey, dirKeySink) <- external (False, False, False, False)
  withWindow width height title $ \win -> do
    initGL width height
    network <- start $ do
      player <- transfer initialPlayer (\s dk -> movePlayer width height s dk) dirKey
      return $ renderFrame win <$> player
    fix $ \loop -> do
      readInput win dirKeySink
      join network
      threadDelay 20000
      esc <- keyIsPressed win Key'Escape
      unless esc loop
    exitSuccess

initGL :: Int -> Int -> IO ()
initGL width height = do
  clearColor $= Color4 1 1 1 1
  ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

readInput :: Window -> (Movement -> IO ()) -> IO ()
readInput window dirKeySink = do
  GLFW.pollEvents
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  dirKeySink (l, r, u, d)

movePlayer :: Int -> Int -> Movement -> Player -> Player
movePlayer _ _ (True, _, _, _) player@(Player (Vector2 xpos ypos) pSize inc)
  | xpos <= pSize / 2 = player
  | otherwise = Player (Vector2 (xpos - inc) ypos) pSize inc
movePlayer w _ (_, True, _, _) player@(Player (Vector2 xpos ypos) pSize inc)
  | xpos >= ((fromIntegral w) - pSize / 2) = player
  | otherwise = Player (Vector2 (xpos + inc) ypos) pSize inc
movePlayer _ h (_, _, True, _) player@(Player (Vector2 xpos ypos) pSize inc)
  | ypos >= ((fromIntegral h) - pSize / 2) = player
  | otherwise = Player (Vector2 xpos (ypos + inc)) pSize inc
movePlayer _ _ (_, _, _, True) player@(Player (Vector2 xpos ypos) pSize inc)
  | ypos <= pSize / 2 = player
  | otherwise = Player (Vector2 xpos (ypos - inc)) pSize inc
movePlayer _ _ _ player = player

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = fmap isPress $ GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

renderFrame :: Window -> Player -> IO ()
renderFrame window (Player (Vector2 xpos ypos) pSize _) = do
  let half = pSize / 2
  clear [ColorBuffer]
  color $ Color4 0 1 0 (1 :: GLdouble)
  renderPrimitive Quads $ do
    vertex $ Vertex2 (xpos - half) (ypos - half)
    vertex $ Vertex2 (xpos + half) (ypos - half)
    vertex $ Vertex2 (xpos + half) (ypos + half)
    vertex $ Vertex2 (xpos - half) (ypos + half)
  color $ Color4 1 1 1 (1 :: GLdouble)
  flush
  GLFW.swapBuffers window
