module Render where

import Model
import Graphics.Rendering.OpenGL hiding (Rect)
import Control.Monad
import qualified Data.Set as Set

type Point = Vertex2 GLdouble
type Rect = (Point,Point)

render :: Rect -> Model -> IO ()
render r m = do
    drawTimeMarker
    mapM_ drawButton [(s,c) | s <- [0..steps-1], c <- [0..channels-1]]

  where 
    steps = m_stepRange m
    channels = length (m_channels m)

    drawTimeMarker :: IO ()
    drawTimeMarker = do
        fillRoundedRect mcolor radius box
      where
        box = (Vertex2 x y, Vertex2 (x+w) (y+h))
        x,y :: GLdouble
        x =  edgeMargin + fi (t `mod` period) / fi period * (bwidth+gap) * fi steps
        w = bwidth+gap
        y =  edgeMargin - gap/2
        h = fi channels*(bheight+gap)
        t = m_clock m
        period = m_stepRange m * m_stepTime m

    drawButton :: (StepID,ChannelID) -> IO ()
    drawButton (s,c) = when active (fillRoundedRect bcolor radius box)
      where
        box = buttonBox r m (s,c)
        active = Set.member (s,c) (m_triggers m)

    radius = 5
    mcolor = Color3 0.5 0.5 0.5
    bcolor = Color3 0 0.8 0.8

    (bwidth,bheight) = bsize r m

buttonBox :: Rect -> Model -> (StepID,ChannelID) -> Rect
buttonBox r m (s,c) = (Vertex2 x y, Vertex2 (x+bw) (y+bh))
  where
    x = edgeMargin + (fi s) * (gap+bw)
    y = edgeMargin + (fi c) * (gap+bh)
    (bw,bh) = bsize r m

bsize :: Rect -> Model -> (GLdouble,GLdouble)
bsize r m = (bw,bh)
  where
    bw = let n = fi (m_stepRange m) in (width r - 2*edgeMargin - (n-1)*gap)/ n
    bh = let n = fi (length (m_channels m)) in (height r - 2*edgeMargin - (n-1)*gap)/ n

edgeMargin = 10
gap = 10

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

width, height :: Rect -> GLdouble
width  (Vertex2 v1 _,Vertex2 v2 _) = v2 - v1
height (Vertex2 _ v1,Vertex2 _ v2) = v2 - v1

fillRoundedRect :: Color3 GLdouble -> GLdouble -> Rect -> IO ()
fillRoundedRect c radius (v1@(Vertex2 x1 y1),v3@(Vertex2 x2 y2)) = do
    renderPrimitive Polygon $ do
        color $ c
        vertex $ v1
        vertex $ v2
        vertex $ v3
        vertex $ v4
  where
    v2 = Vertex2 x2 y1
    v4 = Vertex2 x1 y2


