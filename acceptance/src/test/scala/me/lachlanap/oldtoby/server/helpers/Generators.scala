package me.lachlanap.oldtoby.server.helpers

import scala.util.Random

trait Generators {
  def server: ServerInterface

  val r = new Random()

  def name(prefix: String): String = prefix + "_" + r.alphanumeric.take(10).mkString

  def nilPipeline: Id = Id("pipeline_nil")
}
