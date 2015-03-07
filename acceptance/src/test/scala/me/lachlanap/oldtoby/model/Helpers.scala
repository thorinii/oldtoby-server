package me.lachlanap.oldtoby.model

import scala.util.{Success, Try, Random}

object Helpers {
  val r = new Random()

  def name(prefix: String): String = prefix + "_" + r.alphanumeric.take(10).mkString
}
