package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class FlicFlacPlayerParams(
    playPamsOurName: String, // ......... default "OurName"
    playPamsOppoName: String, // ........ default "OppoName"
    playPamsScoreToWin: Int, // ......... default 11
    playPamsTurnTime: Int, // ........... default 10 seconds
    playPamsCaptorsTime: Int, // ........ default 5 seconds
    playPamsRandEventProb: Int // ....... default 1 (in 100)
) derives Encoder.AsObject,
      Decoder

object FlicFlacPlayerParams:

  def retrieve(): FlicFlacPlayerParams =
    val cacheConfigOrDefault =
      decode[FlicFlacPlayerParams](org.scalajs.dom.window.localStorage.getItem("FlicFlacPlayerParams")) match
        case Right(playerParams: FlicFlacPlayerParams) =>
          scribe.debug(s"@@@ Retrieved PlayerParams $playerParams")
          playerParams
        case Left(_) =>
          scribe.error("@@@ Retrieve PlayerParams failed - assert default")
          FlicFlacPlayerParams(
            "OurName", // ..... Our name
            "OppoName", // .... Opponents name
            11, // ............ ScoreToWin
            10, // ............ TurnTime
            5, // ............. CaptorsTime
            1 // .............. cfgRandEventProb
          )
    cacheConfigOrDefault
  end retrieve

  def GetNames(): (String, String) =
    val playerParams = retrieve()
    (playerParams.playPamsOurName, playerParams.playPamsOppoName)
  end GetNames

end FlicFlacPlayerParams
