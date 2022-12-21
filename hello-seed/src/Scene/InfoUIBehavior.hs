module Scene.InfoUIBehavior( infoUIAct ) where
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




infoUIAct = ActorAct 
  (ActorUpdate id) 
  (ActorActive $ const True) 
  (ActorRenderIO renderInfoUI)


renderInfoUI :: (MonadIO m) => Scene -> m ()
renderInfoUI s = do
  let ui = infoUI s 
  
  let leftTop = Vec 64 32
      rightTop = Vec (getX (screenSize s) - 64) 32
      space = 32

  renderText s score (textScore ui) leftTop LeftTop DefaultStyle
  renderText s high (textHighScore ui) (leftTop ~+ Vec 0 space) LeftTop DefaultStyle

  renderText s lv (textLevel ui) rightTop RightTop DefaultStyle

  when
    -- Game Over
    (sceneState s == Playing && countAfterDiedPlayer (playerState $ player s) > 0)
    $ renderText s "Game Over" (textGameOver ui)
        (screenSize s `divVec` 2)
        MiddleCenter $ Header $ SDL.V4 255 120 80 255

  when
    -- Title
    (sceneState s == Title)
    $ do
      renderText s "Full Up Blobwov" (textTitle ui)
        (screenSize s `divVec` 2)
        MiddleCenter $ Header $ SDL.V4 200 255 80 255
      renderText s "Press Left Click To Start" (textTitlePas ui)
        (screenSize s `divVec` 2 ~+ Vec 0 128)
        MiddleCenter DefaultStyle


  where
    pr = playingRecord s
    score = "Curr Score :  " ++ show (currScore pr)
    high  = "High Score :  " ++ show (highScore pr)
    lv = "Level :  " ++ show (currLevel pr)


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
          (renderer $ env s) (outlinedMplus24 (fontRsc $ env s))
          (SDL.V4 160 255 120 255) (SDL.V4 120 100 120 255)
          (pack str) tex
        Header color -> updateTextBlendedOutlined
          (renderer $ env s) (outlinedMplus96 (fontRsc $ env s))
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

  renderPreRenderedText (renderer $ env s) tex start'