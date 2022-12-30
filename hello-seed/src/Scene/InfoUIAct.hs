{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Scene.InfoUIAct
( updateInfoUI
, renderInfoUI ) where
import Control.Monad.Cont
import qualified SDL
import Scene.InfoUI
import Rendering
import Vec
import Data.Text
import FontRsc
import Scene.Scene
import Data.IORef
import Scene.Player
import qualified SDL.Font
import Control.Lens
import ImageRsc (ImageRsc(logo_hungry, logo_stuffed))
import qualified SDLWrapper
import qualified SDL.Primitive
import Linear
import AnimUtil (degToRad, convertRGB)




type InfoUIUpdate s =
  ( HasInfoUI s InfoUI )


updateInfoUI :: ( InfoUIUpdate s) => s -> InfoUI
updateInfoUI s =
  let ui = s^.infoUI
  in ui { uiFullness = updateFullnessInfo s }


updateFullnessInfo :: ( InfoUIUpdate s) => s -> UiFullness
updateFullnessInfo s = let f = uiFullness $ s^.infoUI
  in case f of
    UiFullness c ->
      UiFullness $ c + 1


renderInfoUI :: ( MonadIO m ) => Scene -> m ()
renderInfoUI s = do
  let ui = s ^. infoUI

  let leftTop = Vec 64 32
      rightTop = Vec (getX (s ^. (metaInfo . screenSize)) - 64) 32
      space = 32

  renderText s score (textScore ui) leftTop LeftTop DefaultStyle
  renderText s high (textHighScore ui) (leftTop ~+ Vec 0 space) LeftTop DefaultStyle

  renderText s lv (textLevel ui) rightTop RightTop DefaultStyle

  when
    -- Game Over
    (isSceneState Playing s && countAfterDiedPlayer (playerState $ s^.player) > 0)
    $ renderText s "Game Over" (textGameOver ui)
        ((s ^. (metaInfo . screenSize)) `divVec` 2)
        MiddleCenter $ Header $ SDL.V4 255 120 80 255

  when
    -- Playing
    (isSceneState Playing s)
    -- Hungry / Stuffed
    $ renderFullness s


  when
    -- Title
    (isSceneState Title s)
    $ do
      renderText s "Full Up Blobwov" (textTitle ui)
        ((s ^. (metaInfo . screenSize)) `divVec` 2)
        MiddleCenter $ Header $ SDL.V4 200 255 80 255
      renderText s "Press Left Click To Start" (textTitlePas ui)
        ((s ^. (metaInfo . screenSize)) `divVec` 2 ~+ Vec 0 128)
        MiddleCenter DefaultStyle


  where
    pr = s ^. (metaInfo . playingRecord)
    score = "Curr Score :  " ++ show (pr ^. currScore)
    high  = "High Score :  " ++ show (pr ^. highScore)
    lv = "Level :  " ++ show (pr ^. currLevel)


data TextAlign = LeftTop | RightTop | MiddleCenter


data Style = DefaultStyle | Header SDL.Font.Color

-- テキスト描画
renderText :: MonadIO m => Scene -> String -> TextTexCache -> VecInt -> TextAlign -> Style -> m ()
renderText s str (TextTexCache tex buff) start align style = do
  beforeBuff <- liftIO $ readIORef buff

  when
    -- キャッシュを取ってテキストに変化があるときだけ描画する
    (beforeBuff /= str)
    $ do
      liftIO $ writeIORef buff str
      case style of
        DefaultStyle -> updateTextBlendedOutlined
          (renderer $ s^.env) (outlinedMplus24 (fontRsc $ s^.env))
          (SDL.V4 160 255 120 255) (SDL.V4 120 100 120 255)
          (pack str) tex
        Header color -> updateTextBlendedOutlined
          (renderer $ s^.env) (outlinedMplus96 (fontRsc $ s^.env))
          color (SDL.V4 80 80 80 255)
          (pack str) tex

  start' <- case align of
        LeftTop -> return start
        RightTop -> do
          texSize <- getSizeOfRenderedText tex
          return $ start ~- Vec (getX texSize) 0
        MiddleCenter -> do
          texSize <- getSizeOfRenderedText tex
          return $ start ~- (texSize `divVec` 2)

  renderPreRenderedText (renderer $ s^.env) tex start'


-- Hungry / Stuffedの表示
renderFullness :: MonadIO m => Scene -> m()
renderFullness s =
  let uiFull = uiFullness $ s ^. infoUI
      r = renderer $ s^.env

      hungry = logo_hungry $ imageRsc $ s ^. env
      stuffed = logo_stuffed $ imageRsc $ s ^. env

      cx = fromIntegral $ getX $ (s ^. (metaInfo . screenSize)) `divVec` 2
      
      sy = 32

  in case uiFull of
    (UiFullness frame) -> do
      let pf = s ^. (player . full)
          isPacman = isPlayerPacman $ playerState $ s^.player
          fullRate = if isPacman 
            then 1
            else (fromIntegral (pf ^. currFull) / fromIntegral (pf ^. maxFull)) :: Float
          
          amp = floor $ abs $ 4 * sin (degToRad $ 10 * frame)
          
          (width, height) = if isPacman
            then (256 + amp * 2, 80) 
            else (240, 64) 

          stuffedW = floor $ fromIntegral width * fullRate

      let sx1 = cx + stuffedW - (width `div` 2)
          w1 = width - stuffedW

          sx2 = cx - (width `div` 2)
          w2 = stuffedW

          lineY = sy + height + 4
          lineW = 5

      -- Hungry
      SDL.copy r hungry Nothing (Just (SDLWrapper.makeRect sx1 sy w1 height))
      when (w1 > 0) $ SDL.Primitive.thickLine r (V2 sx1 lineY) (V2 (sx1 + w1) lineY) lineW (V4 0 152 255 255)

      -- Stuffed
      SDL.copy r stuffed Nothing (Just (SDLWrapper.makeRect sx2 (sy - amp) w2 (height + amp * 2)))
      when 
        (w2 > 0) 
        $ SDL.Primitive.thickLine 
          r (V2 sx2 lineY) (V2 (sx2 + w2) lineY) (lineW + amp) 
          (convertRGB V4 (40 - amp * 10) (255 - amp * 10) (120 + amp * 15))

