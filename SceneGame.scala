package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneGame extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  val name: SceneName = SceneName("Game")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  def subSystems = Set(SSGame("Game"))

// --- Here we have the intrusion of the hex game objects ...
// format: off
  val boardCfg = BoardConfig(
    91,                             // GWIDTH pixel width of graphic
    81,                             // GHEIGHT pixel height of graphic
    Point(260,30),                  // where the (inisible) top left hand corner of the hex grid board is positioned
    3,                              // game size
    70,                             // amount to add to a hex centre x coord to reach the vertical line of the next column
    40,                             // half the amount to add to a hex centre y coord to reach the next hexagon below
    10                              // xcoord of halfway along the top left diagonal line of first hex
  )
// format: on

  var iTick = 0
  var iMove = 0
  var dMsg1 = "1"
  var dMsg2 = "2"
  var dMsg3 = "3"
  var dMsg4 = "4"
  var dMsg5 = "5"

  var throttle = SceneUpdateFragment.empty
  
  // FIXME, eventually we will calculate / fix scaleFactor and boardCfg BasePoint ...
  // ... from window dimensions supplied in main
  var scaleFactor = 1.0

  val hexBoard = HexBoard(boardCfg, scaleFactor)
  
  val pieces = Pieces(
    boardCfg,
    hexBoard
  )

  val highLighter = HighLighter(boardCfg, hexBoard, scaleFactor)

// --- End of hex game intrusion

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {
    case e: PointerEvent.PointerDown =>
      dMsg1 = "PoDown"
      val clickPoint = e.position
      val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        // Pointer Down ... position on the grid
        case Some(pos) =>
          dMsg2 = "GridTrue"
          highLighter.setPos(pos)
          highLighter.shine(true)
          pieces.deselectAllPieces()
          dMsg4 = "SelFalse"
          pieces.findPieceByPos(pos) match
            // Pointer Down ... piece found and on the grid
            case Some(piece) => 
              dMsg3 = "PieceFound"
              piece.setSelected(true)
              dMsg4 = "SelTrue"
            case None        => 
              dMsg3 = "PieceNotFound"
              ;
          end match

        // Pointer Down ... position off the grid
        case None =>
          dMsg2 = "GridFalse"
          ;
      end match
      Outcome(model)

    case e: PointerEvent.PointerUp =>
      dMsg1 = "PoUp"
      val clickPoint = e.position
      val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        // Pointer Up ... The position is on the hex grid
        case Some(pos) =>
          dMsg2 = "GridTrue"
          pieces.findPieceSelected() match
            // Pointer Up ... piece selected and valid position
            case Some(piece) =>
              dMsg3 = "PieceFound"
              if (piece.pCurPos != pos) && (hexBoard.isThisHexBlack(pos) == true) then 
                piece.toggleFlip()
              end if
              piece.setPosition(pos)
              piece.setSelected(false)
              dMsg4 = "SelFalse"

            // Pointer Up ... no piece selected but on the grid
            case None => 
              dMsg3 = "PieceNotFound"
              ;


        // Pointer Up ... the position is off the hex grid
        case None =>
          dMsg2 = "GridFalse"
          pieces.findPieceSelected() match
            // Pointer Up ... we have selected a piece but moved it off the grid
            case Some(piece) =>
              dMsg3 = "PieceFound"
              piece.moveToHome()
              piece.setSelected(false)
              dMsg4 = "SelFalse"

            // Pointer Up ... no piece selected and also off the grid
            case None => 
              dMsg3 = "PieceNotFound"
              ;
          end match

      end match
      // Pointer Up so turn highlighter off
      highLighter.shine(false)
      Outcome(model)

    case _ => 
      Outcome(model)
  }
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] = 
    case FrameTick =>
      iTick += 1
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)
/*
    case e: PointerEvent.PointerMove =>
      if (viewModel.dragOn) then 
        pieces.findPieceSelected() match
          case Some(p) =>
            dMsg5 = "Moving"
            println("@@@ PointerEventMove @ " + e.position)
            viewModel.optDragPos = Some(e.position)

          case None =>
            dMsg5 = "None"
            viewModel.optDragPos = None
      end if
      iMove += 1
      Outcome(viewModel)
*/
    case ButtonRoundEvent =>
      if (viewModel.dragOn) then viewModel.dragOn = false
      else viewModel.dragOn = true
      Outcome(viewModel)

    case _ => 
      Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

//    val textGame = TextBox("Game Scene")
    val textGame = TextBox(dMsg1+":"+dMsg2+":"+dMsg3+":"+dMsg4+":"+dMsg5+":"+iTick+":"+iMove, 1000, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)
    val dragState = if (viewModel.dragOn) then "Drag:ON" else "DRAG:OFF"
    val textDrag = TextBox(dragState, 200, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(110, 60)


    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    if (iTick % 2 == 0) then 
      throttle = SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
                    |+| SceneUpdateFragment(textGame)
                    |+| SceneUpdateFragment(viewModel.dragButton.draw)
                    |+| SceneUpdateFragment(textDrag)
                    |+| SceneUpdateFragment(viewModel.splashButton.draw)
                    |+| SceneUpdateFragment(viewModel.paramsButton.draw)
//                  |+| SceneUpdateFragment(viewModel.gameButton.draw)
                    |+| SceneUpdateFragment(viewModel.resultsButton.draw)
                    |+| hexBoard.paint(scaleFactor)
                    |+| highLighter.paint(scaleFactor)
                    |+| pieces.paint(scaleFactor, viewModel.optDragPos)
    Outcome(throttle)
    

final case class GameSceneViewModel(
  var optDragPos : Option[Point],
  var dragOn : Boolean,
  dragButton: Button,
  splashButton: Button,
  paramsButton: Button,
//  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for {
      bn0 <- dragButton.updateFromPointers(pointers)
      bn1 <- splashButton.updateFromPointers(pointers)
      bn2 <- paramsButton.updateFromPointers(pointers)
//      bn3 <- gameButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
    } yield this.copy( dragButton = bn0, splashButton = bn1, paramsButton = bn2, /*gameButton = bn3,*/ resultsButton = bn4)

object GameSceneViewModel:

  val initial: GameSceneViewModel = 

    GameSceneViewModel(

      None,   // we have no last position of the pointer recorded

      false,  // the drag option is not switched on

      Button (
        buttonAssets = GameAssets.buttonRoundAssets,
        bounds = Rectangle(20, 40, 90, 80),
        depth = Depth(6),
      ).withUpActions(ButtonRoundEvent),

      Button (
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 120, 240, 80),
        depth = Depth(6)
        ).withUpActions(ButtonSplashEvent),

      Button (
        buttonAssets = GameAssets.buttonParamsAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonParamsEvent),
/*-
      Button (
        buttonAssets = GameAssets.buttonGameAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonGameEvent),
*/
      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(20, 320, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
    )

/* Adjusting buttons requires the following...
1) Add/Remove Button from Object GameSceneViewModel
2) Adjust positions of buttons in Object GameSceneViewModel
3) Add/Remove pointer handling in case class GameSceneViewModel / update
4) Add/Remove appropriate SceneUpdateFragment in present / outcome

 */    
    