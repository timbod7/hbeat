module Model where

import qualified Data.Set as Set
import Data.Maybe

type LoopID = Int
type StepID = Int
type ChannelID = Int
type Time = Int
type TimeInterval = Int

type Trigger = (LoopID,StepID,ChannelID)

data Model = Model {
    m_channels :: [ChannelID],
    m_stepRange :: StepID,
    m_loopRange :: LoopID,

    m_triggers :: Set.Set Trigger,
    m_loop :: LoopID, 

    m_stepTime :: TimeInterval,
    m_refreshTime :: TimeInterval,
    m_repaintOffset :: TimeInterval,

    -- transient stuff
    m_prevLoop :: Maybe (Time,LoopID)
}
   deriving (Show)

data Action = Repaint
            | FlipBuffer
            | Play ChannelID
   deriving (Show,Eq)

nextEvent :: Time -> Model -> (Time,[Action])
nextEvent now m = (now',actions)

  where
    (now',actions) = fromJust $ chooseActions [ nextRepaint,
                                                nextRefresh,
                                                nextTriggers ]
    nextRepaint = next (m_refreshTime m) (m_repaintOffset m) (const [Repaint])
    nextRefresh = next (m_refreshTime m) 0 (const [FlipBuffer])
    nextTriggers = next (m_stepTime m)  0 getTriggers

    getTriggers si = [Play cid | cid <- m_channels m, 
                      Set.member (m_loop m, si `mod`m_stepRange m,cid) (m_triggers m)]

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

defaultModel = Model {
    m_channels = [0..3],
    m_stepRange = 16,
    m_loopRange = 4,

    m_triggers = Set.fromList [],
    m_loop = 0,

    m_stepTime = 150,
    m_refreshTime = 10,
    m_repaintOffset = (-5),

    m_prevLoop = Nothing
}

events :: Time -> Model -> [ (Time,[Action]) ]
events t0 m = let (t,as) = nextEvent t0 m in (t,as):events t m

loopTriggers :: LoopID -> Model -> [Trigger]
loopTriggers l m = [(l,s,c) | s <- [0..m_stepRange m-1], c <- [0..channels-1]]
  where 
    channels = length (m_channels m)

updateTrigger :: (Bool -> Bool) -> Trigger -> Model -> Model
updateTrigger ufn t m = if ufn (Set.member t ts)
                            then m{m_triggers=Set.insert t ts}
                            else m{m_triggers=Set.delete t ts}
  where
    ts = m_triggers m    