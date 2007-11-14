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
    drawTimeMarker = fillRoundedRect mcolor radius box
      where
        box = (Vertex2 x y, Vertex2 (x+w) (y+h))
        x,y :: GLdouble
        x =  edgeMargin + (t - fi (floor t)) / period - gap/2
        w = bwidth+gap
        y =  edgeMargin - gap/2
        h = fi channels*(bheight+gap)
        t = fi (m_clock m) / period
        period = fi (m_stepRange m * m_stepTime m)

    drawButton :: (StepID,ChannelID) -> IO ()
    drawButton (s,c) = when active (fillRoundedRect bcolor radius box)
      where
        active = Set.member (s,c) (m_triggers m)
        x = edgeMargin + (fi s) * (gap+bwidth)
        y = edgeMargin + (fi c) * (gap+bheight)
        box = (Vertex2 x y, Vertex2 (x+bwidth) (y+bheight))

    edgeMargin = 10
    gap = 10
    radius = 5
    mcolor = Color3 0.5 0.5 0.5
    bcolor = Color3 0 0.8 0.8

    bwidth = let n = fi steps in (width r - 2*edgeMargin - (n-1)*gap)/ n
    bheight = let n = fi channels in (height r - 2*edgeMargin - (n-1)*gap)/ n
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


