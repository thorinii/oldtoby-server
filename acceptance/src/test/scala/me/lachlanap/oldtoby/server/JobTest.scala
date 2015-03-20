package me.lachlanap.oldtoby.server

import me.lachlanap.oldtoby.server.helpers.Job

class JobTest extends UnitSpec {
  "creating a job" should "return the job's id" in {
    val job = server.createJob(name = name("job"), pipeline = nilPipeline)

    job.right.value shouldBe an[Job]
  }

  it should "put it in the list of jobs" in {
    val job = createJob()

    server.getJobs should contain(job)
  }
}
