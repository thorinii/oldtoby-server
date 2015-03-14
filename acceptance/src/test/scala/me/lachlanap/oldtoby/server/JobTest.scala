package me.lachlanap.oldtoby.server

import me.lachlanap.oldtoby.server.helpers.Id

class JobTest extends UnitSpec {
  "creating a job" should "return the job's id" in {
    val job = server.createJob(name = name("job"), pipeline = nilPipeline)

    job.right.value shouldBe an[Id]
  }
}
