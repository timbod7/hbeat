module Render where

import Model
import Graphics.Rendering.OpenGL hiding (Rect)
import Control.Monad
import qualified Data.Set as Set

type Point = Vertex2 GLdouble
type Rect = (Point,Point)

data Geometry = Geometry {
    g_size :: Size,
    g_tbox :: (StepID,ChannelID) -> Rect,
    g_lbox :: LoopID -> Rect,
    g_bwidth :: GLdouble,
    g_bheight :: GLdouble,
    g_edgeMargin :: GLdouble,
    g_gap :: GLdouble
}

geometry :: Size -> Model -> Geometry
geometry sz@(Size w h) m = Geometry {
    g_size = sz,
    g_tbox = boxfn,
    g_lbox = \l -> boxfn ((m_stepRange m - m_loopRange m) `div` 2 + l,channels), 
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
    bh = let n = fi (length (m_channels m) + 1) in (height r - 2*edgeMargin - (n-1)*gap)/ n
    edgeMargin = 10
    gap = 10
    channels = length (m_channels m)

render :: Geometry -> Time -> Model -> IO ()
render g t m = do
    drawTimeMarker
    mapM_ drawButton (activeTriggers m)
    mapM_ drawLoopButton [0..m_loopRange m - 1]
  where 
    steps = m_stepRange m
    stepTime = m_stepTime m
    channels = length (m_channels m)
    period = steps * stepTime

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

    drawButton :: Trigger -> IO ()
    drawButton (l,s,c) = if active
                           then fillRoundedRect (bcolorf s) radius bbox
                           else lineRoundedRect bcolor1 radius bbox
      where
        bbox = g_tbox g (s,c)
        active = Set.member (l,s,c) (m_triggers m)

    drawLoopButton l = do
        if l == m_loop m
          then fillRoundedRect bcolor2a radius bbox
          else lineRoundedRect bcolor1 radius  bbox
      where
        bbox = g_lbox g l

    radius = 5
    mcolor = Color3 0.5 0.5 0.5
    bcolor1 = Color3 0 0.5 0.5
    bcolor2a = Color3 0.0 0.8 0.8
    bcolor2b = Color3 1.0 0.5 0.5
    bcolorf s = if tx >= 0 && tx <= fade
                  then blendc (fi tx / fi fade) bcolor2a bcolor2b
                  else bcolor2a
      where tx = (t `mod` period) - s * stepTime
            fade = stepTime * 4

    (bwidth,bheight) = (g_bwidth g, g_bheight g)

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

width, height :: Rect -> GLdouble
width  (Vertex2 v1 _,Vertex2 v2 _) = v2 - v1
height (Vertex2 _ v1,Vertex2 _ v2) = v2 - v1

fillRoundedRect c r rect = renderPrimitive Polygon (roundedRectPath c r rect)
lineRoundedRect c r rect = renderPrimitive LineLoop (roundedRectPath c r rect)

roundedRectPath :: Color3 GLdouble -> GLdouble -> Rect -> IO ()
roundedRectPath c r (Vertex2 x1 y1,Vertex2 x2 y2) = do
        color $ c
        vertex $ Vertex2 (x1+r) y1
        vertex $ Vertex2 (x2-r) y1
        vertex $ Vertex2 x2 (y1+r)
        vertex $ Vertex2 x2 (y2-r)
        vertex $ Vertex2 (x2-r) y2
        vertex $ Vertex2 (x1+r) y2
        vertex $ Vertex2 x1 (y2-r)
        vertex $ Vertex2 x1 (y1+r)

blendc :: GLdouble -> Color3 GLdouble -> Color3 GLdouble -> Color3 GLdouble
blendc f (Color3 r1 g1 b1) (Color3 r2 g2 b2) =
  let f' = 1-f in Color3 (f*r1+f'*r2) (f*g1+f'*g2) (f*b1+f'*b2)

inBox :: Point -> Rect -> Bool
inBox (Vertex2 x y) (Vertex2 x0 y0, Vertex2 x1 y1)  = 
    (x >= x0) && (x <= x1) && (y >= y0) && (y <= y1)

click:: Point -> Geometry -> Model -> Model
click p g = (loops.triggers)
  where
    triggers m = foldr (tfn g) m (activeTriggers m)
    tfn g t@(l,s,c) m = if inBox p (g_tbox g (s,c))
                          then updateTrigger not t m
                          else m

    loops m = foldr (lfn g) m [0..m_loopRange m-1]
    lfn g l m = if inBox p (g_lbox g l)
                then m{m_loop=l}
                else m
