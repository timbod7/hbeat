import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL( ($=) )
import qualified Graphics.UI.SDL as SDL
import System.Time
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.State

import Model
import Render
import Sound

data Config = Config {
    c_samples :: [FilePath],
    c_steps :: Int
    } deriving (Read,Show)
    
data World = World {
    now :: IO Time,
    model :: Model,
    geom :: Geometry,
    sounds :: SoundState,
    pending_event :: (Time,[Action])
}

type GState a = StateT World IO a

main :: IO ()
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
    SDL.setCaption "hBeat" "hBeat"

    t0 <- getClockTime
    let clock = tnow t0
    t <- clock

    let ev = nextEvent t m
    let w = World clock m (geometry (GL.Size (fi width) (fi height)) m) sstate ev
    evalStateT mainLoop w
    endSound sstate
    SDL.quit

setVideoMode width height =
    SDL.setVideoMode width height 32 [SDL.OpenGL,SDL.Resizable]

mainLoop :: GState ()
mainLoop = do
    st <- get
    t <- liftIO $ now st
    processActions t
    ev <- liftIO $ SDL.delay 10 >> SDL.pollEvent
    finished <- case ev of
      (SDL.VideoResize w h) -> do
          modify (\st -> st{geom=geometry (GL.Size (fi w) (fi h)) (model st)})
          liftIO $ setVideoMode w h
          redraw
      (SDL.MouseButtonDown x y SDL.ButtonLeft) -> mouseClick t x y >> redraw
      (SDL.MouseButtonDown _ _ SDL.ButtonWheelUp) -> modifySpeed t (\v -> v+5)
      (SDL.MouseButtonDown _ _ SDL.ButtonWheelDown) -> modifySpeed t (\v -> if v > 50 then v-5 else v)
      SDL.VideoExpose -> redraw
      (SDL.KeyDown  SDL.Keysym{SDL.symKey=SDL.SDLK_ESCAPE}) -> return True
      SDL.Quit -> return True
      _ -> return False
    when (not finished) mainLoop
  where
    redraw :: GState Bool
    redraw = do
      st <- get
      liftIO $ display st True
      return False

    modifySpeed :: Time -> (Int->Int) -> GState Bool
    modifySpeed t adj = updateModel (updateStepTime t adj) >> return False

processActions :: Time -> GState ()
processActions t  = do
    st <- get
    let (ta,actions) = pending_event st
    when (t >= ta) $ do
        liftIO $ mapM_ (doAction st) actions
        let ev = nextEvent ta (model st)
        modify (\s -> s{pending_event=ev})
        processActions t
  where
    doAction st (Play c) = playSound (sounds st) c
    doAction st Repaint = display st False
    doAction st FlipBuffer = SDL.glSwapBuffers

mouseClick t x y = modify uf
  where
    uf st = st{model=click t (GL.Vertex2 (fi x) (fi y)) (geom st) (model st)}

updateModel mf = modify (\st -> st{model=mf (model st)})

display :: World -> Bool -> IO ()
display st swapBuffers = do
        t <- now st
        let sz@(GL.Size wWidth wHeight) = g_size (geom st)
        GL.viewport $= (GL.Position 0 0, sz)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (fi wWidth) (fi wHeight) 0
        GL.clear [GL.ColorBuffer]
        GL.preservingMatrix $ do
            render (picture (geom st) t (model st))
        GL.flush
        when swapBuffers SDL.glSwapBuffers

tnow :: ClockTime -> IO Time
tnow t0 = do
    t <- getClockTime
    let td = t `diffClockTimes` t0
    return (fi (tdPicosec td `div` 1000000000) +
            ((tdHour td * 60 + tdMin td) * 60 + tdSec td) * 1000)
