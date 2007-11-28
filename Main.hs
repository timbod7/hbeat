import Graphics.Rendering.OpenGL
import qualified Graphics.UI.SDL as SDL
import Data.IORef
import System.Time
import System.Environment (getArgs)
import Control.Monad

import Model
import Render
import Sound

data Config = Config {
    c_samples :: [FilePath],
    c_steps :: Int
    } deriving (Read,Show)
    
data State = State {
    now :: IO Time,
    model :: Model,
    window_size :: Size,
    sounds :: SoundState
}

main = do
    args <- getArgs
    config <- (liftM read.readFile) (args !! 0)  :: IO Config

    let m = defaultModel{
        m_channels=[0..length (c_samples config)-1],
        m_stepRange=c_steps config
        }
    let width = 50 * m_stepRange m
    let height = 50 * (length (m_channels m))

    SDL.init [SDL.InitEverything]
    sstate <- initSound (c_samples config)
    vinfo <- SDL.getVideoInfo
    SDL.glSetAttribute SDL.glRedSize 8
    SDL.glSetAttribute SDL.glGreenSize 8
    SDL.glSetAttribute SDL.glBlueSize 8
    SDL.glSetAttribute SDL.glDepthSize 16
    SDL.glSetAttribute SDL.glDoubleBuffer 1
    setVideoMode width height

    t0 <- getClockTime
 
    let (m',actions) = nextEvent m
    stv <- newIORef (State (tnow t0) m' (Size (fi width) (fi height)) sstate)
    mainLoop (m_clock m',actions) stv
    endSound sstate
    SDL.quit

setVideoMode width height =
    SDL.setVideoMode width height 32 [SDL.OpenGL,SDL.Resizable]

mainLoop (t1,actions) stv = do
    st <- readIORef stv
    t2 <- now st
    (t1',actions') <-
      if (t1 - t2 <= 0)
        then do processActions st actions
                let (m',actions') = nextEvent (model st)
                writeIORef stv st{model=m'}
                return (m_clock m',actions')
        else return (t1,actions)

    SDL.delay 10
    ev <- SDL.pollEvent
    finished <- case ev of
      (SDL.VideoResize w h) -> do
          modifyIORef stv (\st -> st{window_size=(Size (fi w) (fi h))})
          setVideoMode w h
          redraw st
      (SDL.MouseButtonDown x y SDL.ButtonLeft) -> do
          mouseClick stv x y
          redraw st
      SDL.VideoExpose -> redraw st
      (SDL.KeyDown  SDL.Keysym{SDL.symKey=SDL.SDLK_ESCAPE}) -> return True
      SDL.Quit -> return True
      _ -> return False
    when (not finished) (mainLoop (t1',actions') stv)
  where
    redraw st = do
      display st
      SDL.glSwapBuffers
      return False

processActions st actions = do
    mapM_ doAction actions
  where
    doAction (Play c) = playSound (sounds st) c
    doAction Repaint = display st
    doAction FlipBuffer = SDL.glSwapBuffers

mouseClick stv x y = do
    st <- readIORef stv
    let m0 = model st
    let g = geometry (window_size st) m0
    let m' = click (Vertex2 (fi x) (fi y)) g m0 
    writeIORef stv st{model=m'}
    
display st = do
    let sz@(Size wWidth wHeight) = window_size st
    viewport $= (Position 0 0, sz)
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fi wWidth) (fi wHeight) 0
    clear [ColorBuffer]
    preservingMatrix $ do
        render sz (model st)
    flush

tnow :: ClockTime -> IO Time
tnow t0 = do
    t <- getClockTime
    let td = t `diffClockTimes` t0
    return (fi (tdPicosec td `div` 1000000000) +
            ((tdHour td * 60 + tdMin td) * 60 + tdSec td) * 1000)
