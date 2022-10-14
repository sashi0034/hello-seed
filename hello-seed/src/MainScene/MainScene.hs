module MainScene.MainScene where
import Vec


data Player = Player
  { pos :: VecF
  , animCount :: Int
  }

  
data MainScene = MainScene
  { player :: Player

  }


initialPlayer :: Player
initialPlayer = Player
  { pos = Vec 10 10
  , animCount = 0
  }


initialMainScene :: MainScene
initialMainScene = MainScene
  { player = initialPlayer

  }