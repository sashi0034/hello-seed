module MainScene.MainScene where
import Vec
import MainScene.Player
import MainScene.Background


data MainScene = MainScene
  { player :: Player
  , background :: Background
  }



initialMainScene :: MainScene
initialMainScene = MainScene
  { player = initialPlayer
  , background = initialBackground
  }