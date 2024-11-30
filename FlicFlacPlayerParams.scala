package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class FlicFlacPlayerParams(
    playPams1_Name1: String, // ........... default "Player1"
    playPams2_Name2: String, // ........... default "Player2"
    playPams3_ScoreToWin: Int, // ......... default 11
    playPams4_TurnTime: Int, // ........... default 30 seconds
    playPams5_CaptorsTime: Int, // ........ default 15 seconds
    playPams6_RandEventProb: Int // ....... default 1 (in 100)
) derives Encoder.AsObject,
      Decoder

object FlicFlacPlayerParams:

  def getParams(startupData: FlicFlacStartupData): FlicFlacPlayerParams =

    val name1 = startupData.flicFlacBootData.name1
    val name2 = startupData.flicFlacBootData.name2

    val cacheConfigOrDefault =
      decode[FlicFlacPlayerParams](org.scalajs.dom.window.localStorage.getItem("FlicFlac-Params")) match
        case Right(playerParams: FlicFlacPlayerParams) =>
          val newPlayerParams = playerParams.copy(playPams1_Name1 = name1, playPams2_Name2 = name2)
          newPlayerParams

        case Left(_) =>
          scribe.error("@@@ FlicFlacPlayerParams getParams failed - assert default")
          FlicFlacPlayerParams(
            "Player1", // ..... Our name
            "Player2", // ..... Opponents name
            11, // ............ ScoreToWin
            10, // ............ TurnTime
            5, // ............. CaptorsTime
            1 // .............. cfgRandEventProb
          )
    scribe.debug("@@@ FlicFlacPlayerParams getParams " + cacheConfigOrDefault)
    cacheConfigOrDefault
  end getParams

end FlicFlacPlayerParams
