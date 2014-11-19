module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW (Window, Key(..), KeyState(..))
import qualified Graphics.UI.GLFW as GLFW
import FRP.Elerea.Simple

import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Control.Concurrent

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random

type Pos = Vector2 GLdouble
type Speed = GLdouble
type ObjectSize = GLdouble
type Movement = (Bool, Bool, Bool, Bool)
data Player = Player Pos ObjectSize Speed
data Direction = WalkUp
               | WalkDown
               | WalkLeft
               | WalkRight
  deriving (Show, Eq, Enum, Bounded)
data MonsterStatus = Wander Direction GLdouble
                   | Hunting
  deriving (Show)
data Monster = Monster Pos ObjectSize Speed MonsterStatus

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

wanderDist :: GLdouble
wanderDist = 40

huntingDist :: GLdouble
huntingDist = 100

initialPlayer :: Player
initialPlayer = Player (Vector2 200 200) 20 10

initialMonster :: Monster
initialMonster = Monster (Vector2 400 400) 20 5 (Wander WalkUp wanderDist)

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
    randomGen <- newStdGen
    network <- start $ do
      player <- transfer initialPlayer (\s dk -> movePlayer width height s dk) dirKey
      randomNumber <- stateful (undefined, randomGen) nextRandom
      monster <- transfer2 initialMonster (wanderOrHunt width height) player randomNumber
      return $ renderFrame win <$> player <*> monster -- <*> randomNumber
    fix $ \loop -> do
      readInput win dirKeySink
      join network
      threadDelay 20000
      esc <- keyIsPressed win Key'Escape
      unless esc loop
    exitSuccess
  where
    nextRandom (a, g) = random g

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

renderFrame :: Window -> Player -> Monster -> IO ()
renderFrame window (Player (Vector2 xpos ypos) pSize _) (Monster (Vector2 xmon ymon) monSize _ _)= do
  let half = pSize / 2
  clear [ColorBuffer]
  color $ Color4 0 1 0 (1 :: GLdouble)
  renderPrimitive Quads $ do
    vertex $ Vertex2 (xpos - half) (ypos - half)
    vertex $ Vertex2 (xpos + half) (ypos - half)
    vertex $ Vertex2 (xpos + half) (ypos + half)
    vertex $ Vertex2 (xpos - half) (ypos + half)
  color $ Color4 1 0 0 (1 :: GLdouble)
  renderPrimitive Triangles $ do
    vertex $ Vertex2 (xmon - monSize / 2) (ymon - monSize / 2)
    vertex $ Vertex2 (xmon + monSize / 2) (ymon - monSize / 2)
    vertex $ Vertex2 xmon (ymon + monSize / 2)
  color $ Color4 1 1 1 (1 :: GLdouble)
  flush
  GLFW.swapBuffers window

-- wanderOrHunt :: Player -> () -> Monster -> Monster
wanderOrHunt width height player (randDir, _) monster =
  if close player monster
    then hunt player monster
    else wander width height randDir monster

close :: Player -> Monster -> Bool
close player monster = distance player monster < huntingDist ^ 2

distance :: Player -> Monster -> GLdouble
distance (Player (Vector2 xpos ypos) _ _ ) (Monster (Vector2 xmon ymon) _ _ _) =
  (xpos - xmon) ^ 2 + (ypos - ymon) ^ 2

hunt :: Player -> Monster -> Monster
hunt (Player (Vector2 xpos ypos) _ _) (Monster (Vector2 xmon ymon) monSize monSpeed _) =
  Monster (Vector2 (xmon + (signum (xpos - xmon)) * monSpeed)
                   (ymon + (signum (ypos - ymon)) * monSpeed)) monSize monSpeed Hunting

wander :: Int -> Int -> Direction -> Monster -> Monster
wander _ _ randDir (Monster (Vector2 xmon ymon) mSize mSpeed (Wander _ 0)) =
  Monster (Vector2 xmon ymon) mSize mSpeed (Wander randDir wanderDist)
wander _ _ randDir (Monster (Vector2 xmon ymon) mSize mSpeed Hunting) =
  Monster (Vector2 xmon ymon) mSize mSpeed (Wander randDir wanderDist)
wander width height _ (Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkUp n))
  | ymon < (fromIntegral height - mSize / 2) =
      Monster (Vector2 xmon (ymon + mSpeed)) mSize mSpeed (Wander WalkUp (n - 1))
  | otherwise = Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkDown (n - 1))
wander width height _ (Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkDown n))
  | ymon > mSize / 2 =
      Monster (Vector2 xmon (ymon - mSpeed)) mSize mSpeed (Wander WalkDown (n - 1))
  | otherwise = Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkUp (n - 1))
wander width height _ (Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkLeft n))
  | xmon > mSize / 2 =
      Monster (Vector2 (xmon - mSpeed) ymon) mSize mSpeed (Wander WalkLeft (n - 1))
  | otherwise = Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkRight (n - 1))
wander width height _ (Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkRight n))
  | xmon > (fromIntegral width - mSize / 2) =
      Monster (Vector2 (xmon + mSpeed) ymon) mSize mSpeed (Wander WalkRight (n - 1))
  | otherwise = Monster (Vector2 xmon ymon) mSize mSpeed (Wander WalkLeft (n - 1))
