module MainScene.MainScene where
import MainScene.Player

data MainScene = MainScene
  { player :: Player

  }

initialMainScene = MainScene
  { player = initialPlayer

  }