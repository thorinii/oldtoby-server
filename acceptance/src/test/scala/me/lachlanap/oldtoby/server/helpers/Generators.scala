package me.lachlanap.oldtoby.server.helpers

import scala.util.Random

trait Generators {
  def server: ServerInterface

  val r = new Random()

  def name(prefix: String): String = prefix.capitalize + " " + r.alphanumeric.take(10).mkString

  def nilPipeline: Id = Id("pipeline_nil")

  def createJob(name: String = this.name("Job"), pipeline: Id = nilPipeline) = {
    server.createJob(name, pipeline).right.get
  }
}
