package me.lachlanap.oldtoby.server.helpers

import com.fasterxml.jackson.core.JsonProcessingException
import com.ning.http.client.AsyncHttpClientConfig
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.TestFailedException
import org.scalatest.time.SpanSugar._
import play.api.libs.json.{JsNull, JsResultException, JsValue, Json}
import play.api.libs.ws.WSResponse
import play.api.libs.ws.ning.NingWSClient

import scala.concurrent.Future

/**
 * Performs the HTTP to the server.
 */
class ServerInterface(address: String) extends ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  val timeLimit = 500.millis
  implicit override val patienceConfig = PatienceConfig(timeout = timeLimit, interval = 5.millis)

  private val client = {
    val builder = new AsyncHttpClientConfig.Builder()
    new NingWSClient(builder.build())
  }


  def status: Status =
    get("/status") {
                     case (200, _) => Up()
                     case _ => Down
                   }


  def createJob(name: String, pipeline: Id): Either[Int, Job] = {
    val json = Json.obj("name" -> name,
                        "pipeline" -> pipeline.value)

    post("/job", json) {
                         case (201, result) =>
                           Right(jobFrom(result))
                         case (e, _) => Left(e)
                       }
  }


  def getJobs: List[Job] =
    get("/job") {
                  case (200, result) =>
                    result.as[List[JsValue]].map(jobFrom)
                  case _ => List.empty
                }


  def ingest(job: Id, pages: Int): List[Id] = {
    val json = Json.obj("pages" -> pages)

    post(s"/job/${job.value }/page", json) {
                                             case (201, result) =>
                                               result.as[List[String]].map(Id)
                                             case _ => List.empty
                                           }
  }


  def pushMetadata(page: Id, stage: String, metadata: Map[String, String]) = {
    pushBulkMetadata((page, stage, metadata) :: Nil)
  }

  def pushBulkMetadata(batch: List[(Id, String, Map[String, String])]) = {
    val json = Json.toJson(
      batch.map { case (page, stage, metadata) =>
        Json.obj("page" -> page.value,
                 "stage" -> stage,
                 "metadata" -> Json.toJson(metadata))
                }
    )

    post(s"/metadata", json) {
                               case (201, _) =>
                               case (e, _) => throw new Exception(s"Failed to push metadata; got HTTP $e")
                             }
  }


  def getMetadata(page: Id): Either[Int, Map[String, String]] =
    get(s"/metadata/${page.value }") {
                                       case (200, result) =>
                                         Right(result.as[Map[String, String]])
                                       case (e, _) => Left(e)
                                     }


  private def jobFrom(js: JsValue) = Job(Id((js \ "id").as[String]),
                                         (js \ "name").as[String],
                                         Id((js \ "pipeline").as[String]))


  private def get[A](u: String)(mapper: (Int, JsValue) => A) =
    healF(() => client.url(url(u)).get().map(response(u, "GET", mapper)))

  private def post[A](u: String, data: JsValue)(mapper: (Int, JsValue) => A) =
    healF(() => client.url(url(u)).post(data).map(response(u, "POST", mapper)))

  private def response[A](u: String, method: String, mapper: (Int, JsValue) => A) = (resp: WSResponse) => {
    if (resp.body.isEmpty)
      mapper(resp.status, JsNull)
    else try {
      mapper(resp.status, resp.json)
    } catch {
      case e: JsonProcessingException =>
        throw new TestFailedException(s"When ${method }ing $u, failed to parse body of response:\n${resp.body }", e, 0)
      case e@JsResultException(errs) =>
        throw new TestFailedException(s"When ${method }ing $u, failed to parse body of response:\n${resp.body }\n$errs", e, 0)
    }
  }


  private def healF[A](f: () => Future[A]) = {
    try {
      f().futureValue
    } catch {
      case e: TestFailedException if e.getCause.isInstanceOf[TestFailedException] => throw e.getCause
    }
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

case class Job(id: Id, name: String, pipeline: Id)
