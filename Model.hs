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
    (now',actions) = fromJust $ chooseActions [ nextRepaint,
                                                nextRefresh,
                                                nextTriggers ]
    now = m_clock m
    nextRepaint = next (m_refreshTime m) (m_repaintOffset m) (const [Repaint])
    nextRefresh = next (m_refreshTime m) 0 (const [FlipBuffer])
    nextTriggers = next (m_stepTime m)  0 getTriggers

    getTriggers si = [Play cid | cid <- m_channels m, Set.member (si `mod`m_stepRange m,cid) (m_triggers m)]

    next :: TimeInterval -> TimeInterval -> (Int -> [Action]) -> (Time,[Action])
    next period offset afn = (i * period + offset,afn i)
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
                 (10,2),
                 (10,3),
        (8,0),
        (12,0),
                 (14,1),
                 (14,2),
                 (14,3),
                 (15,3)
    ],
    m_stepTime = 100,
    m_refreshTime = 25,
    m_repaintOffset = (-5),
    m_clock = (-10)
}

events :: Model -> [ (Time,[Action]) ]
events m = let (m',a) = nextEvent m in (m_clock m',a):events m'

