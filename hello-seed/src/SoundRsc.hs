module SoundRsc where

import SDL.Mixer ( Chunk, play )
import Control.Monad.Cont
import qualified SDLWrapper


data SoundRsc =SoundRsc
  { eat_enemy :: Chunk
  , game_start :: Chunk
  , eat_harvest :: Chunk
  , player_damaged :: Chunk
  , pacman_become :: Chunk
  , game_over :: Chunk
  , level_up :: Chunk
  , pacman_blink :: Chunk
  }


withSoundRsc :: (MonadIO m) => (SoundRsc -> m a) -> m()
withSoundRsc op = 
  SDLWrapper.withChunk "./assets/sounds/eat_enemy.mp3" $ \eat_enemy' ->
  SDLWrapper.withChunk "./assets/sounds/game_start.mp3" $ \game_start' ->
  SDLWrapper.withChunk "./assets/sounds/eat_harvest.mp3" $ \eat_harvest' ->
  SDLWrapper.withChunk "./assets/sounds/player_damaged.mp3" $ \player_damaged' ->
  SDLWrapper.withChunk "./assets/sounds/pacman_become.mp3" $ \pacman_become' ->
  SDLWrapper.withChunk "./assets/sounds/game_over.mp3" $ \game_over' ->
  SDLWrapper.withChunk "./assets/sounds/level_up.mp3" $ \level_up' ->
  SDLWrapper.withChunk "./assets/sounds/pacman_blink.mp3" $ \pacman_blink' -> do
  
    void $ op (SoundRsc
      { eat_enemy = eat_enemy'
      , game_start = game_start'
      , eat_harvest = eat_harvest'
      , player_damaged = player_damaged'
      , pacman_become = pacman_become'
      , game_over = game_over'
      , level_up = level_up'
      , pacman_blink = pacman_blink'
      })

