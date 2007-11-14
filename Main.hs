import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Model
import Render

model = test_model

main = do
    (progname, _) <- getArgsAndInitialize
    createWindow "Steps"
    windowSize $= Size (fi (50 * m_stepRange model)) (fi (50 * (length (m_channels model))))
    wstate <- newIORef (Size 0 0)
    displayCallback $= display wstate
    reshapeCallback $= Just (reshape wstate)
    mainLoop

reshape wstate s = do
  viewport $= (Position 0 0, s)
  writeIORef wstate s
  putStrLn (show ("size",s))
  postRedisplay Nothing


display wstate = do
    (Size wWidthi wHeighti) <- readIORef wstate
    let wWidth = fromIntegral wWidthi
    let wHeight = fromIntegral wHeighti
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 wWidth 0 wHeight
    clear [ColorBuffer]
    preservingMatrix $ do
        render (Vertex2 0 0, Vertex2 wWidth wHeight) model
    flush

fi = fromIntegral