module Render where

import Model
import Graphics.Rendering.OpenGL hiding (Rect)
import Control.Monad
import qualified Data.Set as Set

type Point = Vertex2 GLdouble
type Rect = (Point,Point)

data Geometry = Geometry {
    g_bbox :: (StepID,ChannelID) -> Rect,
    g_bwidth :: GLdouble,
    g_bheight :: GLdouble,
    g_edgeMargin :: GLdouble,
    g_gap :: GLdouble
}

geometry :: Size -> Model -> Geometry
geometry (Size w h) m = Geometry {
    g_bbox = boxfn,
    g_bwidth = bw,
    g_bheight = bh,
    g_edgeMargin = edgeMargin,
    g_gap = gap
    }
  where
    r = (Vertex2 0 0, Vertex2 (fi w) (fi h))
    boxfn (s,c) = (Vertex2 x y, Vertex2 (x+bw) (y+bh))
      where
        x = edgeMargin + (fi s) * (gap+bw)
        y = edgeMargin + (fi c) * (gap+bh)
    bw = let n = fi (m_stepRange m) in (width r - 2*edgeMargin - (n-1)*gap)/ n
    bh = let n = fi (length (m_channels m)) in (height r - 2*edgeMargin - (n-1)*gap)/ n
    edgeMargin = 10
    gap = 10

render :: Size -> Model -> IO ()
render sz m = do
    drawTimeMarker
    mapM_ drawButton (allTriggers m)
  where 
    steps = m_stepRange m
    channels = length (m_channels m)

    g = geometry sz m
    em = g_edgeMargin g
    gap = g_gap g

    drawTimeMarker :: IO ()
    drawTimeMarker = do
        fillRoundedRect mcolor radius box
      where
        box = (Vertex2 x y, Vertex2 (x+w) (y+h))
        x,y :: GLdouble
        x =  em + fi (t `mod` period) / fi period * (bwidth+gap) * fi steps
        w = bwidth+gap
        y =  em - gap/2
        h = fi channels*(bheight+gap)
        t = m_clock m
        period = m_stepRange m * m_stepTime m

    drawButton :: (StepID,ChannelID) -> IO ()
    drawButton (s,c) = when active (fillRoundedRect bcolor radius (g_bbox g (s,c)))
      where
        active = Set.member (s,c) (m_triggers m)

    radius = 5
    mcolor = Color3 0.5 0.5 0.5
    bcolor = Color3 0 0.8 0.8

    (bwidth,bheight) = (g_bwidth g, g_bheight g)

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


inBox :: Point -> Rect -> Bool
inBox (Vertex2 x y) (Vertex2 x0 y0, Vertex2 x1 y1)  = 
    (x >= x0) && (x <= x1) && (y >= y0) && (y <= y1)