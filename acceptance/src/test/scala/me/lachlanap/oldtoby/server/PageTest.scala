package me.lachlanap.oldtoby.server

class PageTest extends UnitSpec {
  "requesting an ingest" should "return the correct number of page ids" in {
    val job = createJob()
    val pageIds = server.ingest(job.id, 10)

    pageIds.length shouldBe 10
  }
}
