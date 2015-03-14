package me.lachlanap.oldtoby.server

import me.lachlanap.oldtoby.server.helpers.{Generators, ServerInterface}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

/**
 * Base test class for tests.
 */
abstract class UnitSpec extends FlatSpec
                                with Matchers
                                with OptionValues
                                with EitherValues
                                with Inside
                                with Inspectors
                                with BeforeAndAfterAll
                                with ScalaFutures
                                with Generators {

  private[this] var _serverAddress: Option[String] = None

  def serverAddress = _serverAddress.get


  lazy val server = new ServerInterface(serverAddress)


  override def beforeAll(configMap: ConfigMap) = {
    _serverAddress = Some(configMap.getWithDefault[String]("server", "http://localhost:8273/"))
  }

  override def afterAll(configMap: ConfigMap) = {}
}
