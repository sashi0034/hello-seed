module MainScene.MainScene where
import Vec
import MainScene.Player
import MainScene.Background
import MainScene.MeteorManager (MeteorManager, initialMeteorManager)


data MainScene = MainScene
  { player :: Player
  , background :: Background
  , meteorManager :: MeteorManager
  , screenSize :: VecInt
  }


initialMainScene :: VecInt -> MainScene
initialMainScene screenSize' = MainScene
  { player = initialPlayer
  , background = initialBackground
  , meteorManager = initialMeteorManager
  , screenSize = screenSize'
  }