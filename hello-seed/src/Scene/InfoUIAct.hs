module Scene.InfoUIAct( infoUIAct ) where
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




infoUIAct :: ActorAct
infoUIAct = ActorAct
  (ActorUpdate id)
  (ActorActive $ const True)
  (ActorRenderIO renderInfoUI)


renderInfoUI :: (MonadIO m) => Scene -> m ()
renderInfoUI s = do
  let ui = s ^. infoUI 

  let leftTop = Vec 64 32
      rightTop = Vec (getX (s^.screenSize) - 64) 32
      space = 32

  renderText s score (textScore ui) leftTop LeftTop DefaultStyle
  renderText s high (textHighScore ui) (leftTop ~+ Vec 0 space) LeftTop DefaultStyle

  renderText s lv (textLevel ui) rightTop RightTop DefaultStyle

  when
    -- Game Over
    (isSceneState Playing s && countAfterDiedPlayer (playerState $ s^.player) > 0)
    $ renderText s "Game Over" (textGameOver ui)
        ((s^.screenSize) `divVec` 2)
        MiddleCenter $ Header $ SDL.V4 255 120 80 255

  when
    -- Title
    (isSceneState Title s)
    $ do
      renderText s "Full Up Blobwov" (textTitle ui)
        ((s^.screenSize) `divVec` 2)
        MiddleCenter $ Header $ SDL.V4 200 255 80 255
      renderText s "Press Left Click To Start" (textTitlePas ui)
        ((s^.screenSize) `divVec` 2 ~+ Vec 0 128)
        MiddleCenter DefaultStyle


  where
    pr = s ^. sceneMeta ^. playingRecord
    score = "Curr Score :  " ++ show (pr ^. currLevel)
    high  = "High Score :  " ++ show (pr ^. currLevel)
    lv = "Level :  " ++ show (pr ^. currLevel)


data TextAlign = LeftTop | RightTop | MiddleCenter


data Style = DefaultStyle | Header SDL.Font.Color


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