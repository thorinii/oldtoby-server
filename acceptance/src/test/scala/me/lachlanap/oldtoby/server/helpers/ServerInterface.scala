package me.lachlanap.oldtoby.server.helpers

import com.fasterxml.jackson.core.JsonParseException
import com.ning.http.client.AsyncHttpClientConfig
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.TestFailedException
import org.scalatest.time.SpanSugar._
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.libs.ws.ning.NingWSClient

/**
 * Performs the HTTP to the server.
 */
class ServerInterface(address: String) extends ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  val timeLimit = 500.millis
  implicit override val patienceConfig = PatienceConfig(timeout = timeLimit, interval = 10.millis)

  private val client = {
    val builder = new AsyncHttpClientConfig.Builder()
    new NingWSClient(builder.build())
  }


  def status: Status = get("/status") {
                                        case (200, _) => Up()
                                        case _ => Down
                                      }.recover { case e: Exception => DownError(e) }.futureValue


  def createJob(name: String, pipeline: Id): Either[Unit, Id] = {
    val json = Json.obj("name" -> name,
                        "pipeline" -> pipeline.value)

    post("/job", json) {
                         case (201, result) =>
                           val id = Id((result \ "id").as[String])
                           Right(id)
                         case _ => Left(())
                       }.futureValue
  }


  private def get[A](u: String)(mapper: (Int, JsValue) => A) = {
    client.url(url(u)).get().map(resp => {
      if (resp.body.isEmpty)
        mapper(resp.status, JsNull)
      else try {
        mapper(resp.status, resp.json)
      } catch {
        case e: JsonParseException =>
          throw new TestFailedException(s"When GETting $u, failed to parse body of response:\n${resp.body }", e, 0)
      }
    })
  }

  private def post[A](u: String, data: JsValue)(mapper: (Int, JsValue) => A) = {
    client.url(url(u)).post(data).map(resp => {
      if (resp.body.isEmpty)
        mapper(resp.status, JsNull)
      else try {
        mapper(resp.status, resp.json)
      } catch {
        case e: JsonParseException =>
          throw new TestFailedException(s"When POSTing $u, failed to parse body of response:\n${resp.body }", e, 0)
      }
    })
  }

  private def url(u: String) = {
    val root = if (address.endsWith("/")) address else address + "/"
    val sub = if (u.startsWith("/")) u.substring(1) else u

    root + sub
  }
}


sealed trait Status

case class DownError(why: Exception) extends Status

case object Down extends Status

case class Up() extends Status


case class Id(value: String)