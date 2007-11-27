module Sound where

import qualified Graphics.UI.SDL.Mixer as Mix
import Graphics.UI.SDL.Audio
import Control.Monad
import Data.IORef

type SoundState = IORef [ Mix.Chunk ]

initSound :: [FilePath] -> IO SoundState
initSound soundFiles = do
    Mix.openAudio 22050 AudioS16Sys 2 4096
    sounds <- mapM Mix.loadWAV soundFiles
    newIORef sounds

playSound :: SoundState -> Int -> IO ()
playSound sstate cid = do 
    sounds <- readIORef sstate
    when (cid < length sounds)
         (Mix.playChannel (-1) (sounds !! cid) 0 >> return ())

endSound :: SoundState -> IO ()
endSound sstate = Mix.closeAudio