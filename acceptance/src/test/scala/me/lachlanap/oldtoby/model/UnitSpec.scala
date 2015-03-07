package me.lachlanap.oldtoby.model

import org.scalatest._

/**
 * Base test class for tests.
 */
abstract class UnitSpec extends FlatSpec
                        with Matchers
                        with OptionValues
                        with Inside
                        with Inspectors
                        with BeforeAndAfterAll {

  private[this] var _server: Option[String] = None
  def server = _server.get

  override def beforeAll(configMap: ConfigMap) = {
    _server = Some(configMap.getWithDefault[String]("server", "http://localhost:9001/"))
  }

  override def afterAll(configMap: ConfigMap) = {}
}
