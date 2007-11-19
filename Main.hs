import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Time
import Control.Monad

import Model
import Render

data State = State {
    now :: IO Time,
    model :: Model,
    window_size :: Size
}

main = do
    let m = test_model
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
  

    createWindow "Steps"
    windowSize $= Size (fi (50 * m_stepRange m))
                       (fi (50 * (length (m_channels m))))
    t0 <- getClockTime
    stv <- newIORef (State (tnow t0) m (Size 0 0))
    displayCallback $= display stv
    reshapeCallback $= Just (reshape stv)
    keyboardMouseCallback $= Just (keymouse stv)
    createEventTimeout stv
    mainLoop
    
createEventTimeout stv = do
    st <- readIORef stv
    let (m,actions) = nextEvent (model st)
    writeIORef stv st{model=m}
    t <- now st
    let td = m_clock m - t
    if td <= 0 
      then do
        processEvents stv actions
        createEventTimeout stv
      else do 
        addTimerCallback td $ do
            processEvents stv actions
            createEventTimeout stv
    return ()

processEvents stv actions = do
    mapM_ doAction actions
  where
    doAction Repaint = do
        st <- readIORef stv
        postRedisplay Nothing
    doAction FlipBuffer = swapBuffers
    doAction a@(Play _) = print a

reshape stv s = do
    modifyIORef stv (\st -> st{window_size=s})
    viewport $= (Position 0 0, s)
    postRedisplay Nothing

keymouse stv (MouseButton LeftButton) Down modifiers (Position x y) = do
    st <- readIORef stv
    let m0 = model st
    let g = geometry (window_size st) m0
    let m' = foldr (bfn g) m0 (allTriggers m0)
    writeIORef stv st{model=m'}
  where
    p = Vertex2 (fi x) (fi y)
    bfn :: Geometry -> Trigger -> Model -> Model
    bfn g t m = if inBox p (g_bbox g t)
                    then updateTrigger not t m
                    else m
    
keymouse stv key keystate modifiers pos = return ()

display stv = do
    st <- readIORef stv
    let sz@(Size wWidth wHeight) = window_size st
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
