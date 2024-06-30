package game

import indigo.*
import indigo.scenes.*

object SceneGame extends Scene[StartUpData, Model, ViewModel]:

  type SceneModel = Model
  type SceneViewModel = ViewModel

  val name: SceneName = SceneName("SceneGame")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, SceneViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters = EventFilters.Permissive

  val subSystems = Set(SSGame("Game"))

  def updateModel(
      context: SceneContext[StartUpData],
      model: SceneModel
  ): GlobalEvent => Outcome[SceneModel] =
    case ButtonsTestEvent =>
      println("Game-ButtonsTestEvent")
      Outcome(model)
    case _ => Outcome(model)
  end updateModel

  def updateViewModel(
      context: SceneContext[StartUpData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    _ => Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.
  def present(
      context: SceneContext[StartUpData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome {

      val textGame = TextBox("Game Scene", 400, 40)
        .withColor(RGBA.Cyan)
        .withFontSize(Pixels(30))
        .moveTo(300, 0)

      SceneUpdateFragment(
        Shape
          .Box(
            Rectangle(0, 0, 3000, 2000),
            Fill.LinearGradient(Point(0), RGBA.SlateGray, Point(3000, 2000), RGBA.Crimson)
          )
      )
        |+| SceneUpdateFragment(textGame)
        |+| SceneUpdateFragment(viewModel.buttonSplash.draw)
        |+| SceneUpdateFragment(viewModel.buttonParams.draw)
//      |+| SceneUpdateFragment(viewModel.buttonGame.draw)
        |+| SceneUpdateFragment(viewModel.buttonResults.draw)
    }
end SceneGame
