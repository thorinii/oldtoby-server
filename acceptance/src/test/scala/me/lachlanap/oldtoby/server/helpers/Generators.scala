package me.lachlanap.oldtoby.server.helpers

import scala.util.Random

trait Generators {
  val r = new Random()

  def name(prefix: String): String = prefix + "_" + r.alphanumeric.take(10).mkString
}
