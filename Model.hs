module Model where

import qualified Data.Set as Set
import Data.Maybe

type ChannelID = Int
type StepID = Int
type Time = Int
type TimeInterval = Int

data Model = Model {
    m_channels :: [ChannelID],
    m_stepRange :: StepID,
    m_triggers :: Set.Set (StepID,ChannelID),

    m_stepTime :: TimeInterval,
    m_refreshTime :: TimeInterval,
    m_repaintOffset :: TimeInterval,
    m_clock :: Time
}
   deriving (Show)

data Action = Repaint
            | FlipBuffer
            | Play ChannelID
   deriving (Show,Eq)

nextEvent :: Model -> (Model,[Action])
nextEvent m = (m{m_clock=now'},actions)

  where
    (now',actions) = fromJust $ chooseActions [ (tRedraw,[Repaint]),
                                                (tRefresh,[FlipBuffer]),
                                                (tStep,nextTriggers) ]
    now = m_clock m
    (tRedraw,_) = nextT (m_refreshTime m) (m_repaintOffset m) now
    (tRefresh,_) = nextT (m_refreshTime m) 0 now
    (tStep,si) = nextT (m_stepTime m) 0 now
    nextStepID = si `mod` m_stepRange m
    nextTriggers = [Play cid | cid <- m_channels m, Set.member (nextStepID,cid) (m_triggers m)] 

nextT :: TimeInterval -> TimeInterval -> Time -> (Time,Int)
nextT period offset now = (i * period + offset,i)
  where
    i = (now - offset) `div` period + 1

chooseActions :: [ (Time,[Action]) ] -> Maybe (Time,[Action])
chooseActions = foldr f Nothing
  where
    f ev Nothing = Just ev
    f ev1@(t1,a1) (Just ev2@(t2,a2)) | t1 < t2 = Just ev1
                                     | t2 < t1 = Just ev2
                                     | otherwise = Just (t1,a1++a2)

test_model = Model {
    m_channels = [0..3],
    m_stepRange = 16,
    m_triggers = Set.fromList [
        (0,0),
        (4,0),
                 (10,1),
        (8,0),
        (12,0),
                 (14,1)
    ],
    m_stepTime = 100,
    m_refreshTime = 25,
    m_repaintOffset = (-5),
    m_clock = (-26)
}

events :: Model -> [ (Time,[Action]) ]
events m = let (m',a) = nextEvent m in (m_clock m',a):events m'

