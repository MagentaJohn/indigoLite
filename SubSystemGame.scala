package game

import scala.scalajs.js
import indigo.*

import java.util.Timer
import indigo.platform.networking.Network
import io.github.quafadas.peerscalajs.Peer
import io.github.quafadas.peerscalajs.DataConnection
import io.circe.syntax.*
import io.circe.parser.decode

case class FlicFlacNewInformationEvent(ffgm: FlicFlacGameModel) extends SubSystemEvent

sealed trait WebRtcEvent extends GlobalEvent

object WebRtcEvent:
  case object MakePeerConnection extends WebRtcEvent
  case class RecievedData(ffgm: FlicFlacGameModel) extends WebRtcEvent
  case class SendGameData(ffgm: FlicFlacGameModel) extends WebRtcEvent

end WebRtcEvent

final case class SSGame(initialMessage: String) extends SubSystem[FlicFlacGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None

  var latestUpdate: Option[FlicFlacGameModel] = None

  type EventType = GlobalEvent

  type SubSystemModel = Unit
  type ReferenceData = FlicFlacGameModel
  val id: SubSystemId = SubSystemId("SubSystemGame")

  var peer: Option[Peer] = None
  var dataConnection: Option[DataConnection] = None

  val eventFilter: GlobalEvent => Option[EventType] = {
    case ft: FrameTick  => Some(ft)
    case e: WebRtcEvent => Some(e)
    case _              => None
  }

  // Extra line here, as mandated by indigo's SubSystem.scala. Yet it is not in the examples!!!
  def reference(flicFlacGameModel: FlicFlacGameModel): FlicFlacGameModel = flicFlacGameModel

  def initialModel: Outcome[Unit] =
    scribe.debug("@@@ SubSystemGame initialModel")

    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] = {
    case WebRtcEvent.SendGameData(ffgm) =>
      scribe.debug("@@@ SubSystemGame WebRtcEvent.SendGameData")
      val toSend = ffgm.asJson.noSpaces
      scribe.debug(toSend)
      conn.foreach(_.send(js.JSON.parse(toSend)))
      Outcome(())

    case WebRtcEvent.MakePeerConnection =>
      scribe.debug("@@@ SubSystemGame WebRtcEvent.MakePeerConnection")
      println("MakePeerConnection")
      println(s"data ${context.reference}")
      peer = Some(Peer(id = context.reference.ourName))
      peer.foreach(
        _.on(
          "connection",
          (c: DataConnection) =>
            scribe.debug(s"@@@ SubSystemGame We made a connection! with $c")
            conn = Some(c)
            c.on(
              "data",
              (data: js.Object) =>
                val str = js.JSON.stringify(data)
                scribe.debug(s"@@@ SubSystemGame received data $data ")

                val ffgm = decode[FlicFlacGameModel](str)
                  .fold(
                    e =>
                      scribe.error(s"Error decoding data: $e")
                      throw new Exception("Error decoding data")
                    ,
                    identity
                  )
                latestUpdate = Some(ffgm)
            )
        )
      )
      peer.foreach { p =>
        scribe.info(s"@@@ SubSystemGame Connecting to ${context.reference.oppoName}")
        val dataConn = p.connect(context.reference.oppoName)
        conn = Some(dataConn)
      }
      Outcome(())

    case FrameTick =>
      val outcome = latestUpdate.fold {
        Outcome(())
      } { ffgm =>
        Outcome(()).addGlobalEvents(WebRtcEvent.RecievedData(ffgm))
      }
      latestUpdate = None
      outcome
  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame

/*---
// when p rxes a connection event ... this function fires
    val p = new Peer(id = id)
    p.on("connection", (data: DataConnection) => {

        data.on("data", (data: js.Object) => {
          println(s"Received data: ${js.JSON.stringify(data)}")
        })
        dataConnection.set(Some(data))  // sets our data connection variable to be that connection
        println(s"Received data connection from ${data.peer}")
    })

// send your opponents id
  val data = p.connect(inId) // p.connect(s2)
          data.on("data", (data: js.Object) => {
            println(s"Received data: ${js.JSON.stringify(data)}")
          })
          dataConnection.set(Some(data))   

// sending a message between 
  case (dataConn, msg) =>
    println(s"sending message: $msg")
    dataConn.foreach(_.send(msg))         
*/