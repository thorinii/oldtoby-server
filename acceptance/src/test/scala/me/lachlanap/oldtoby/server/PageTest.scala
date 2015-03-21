package me.lachlanap.oldtoby.server

class PageTest extends UnitSpec {
  "requesting an ingest" should "return the correct number of page ids" in {
    val job = createJob()
    val pageIds = server.ingest(job.id, 10)

    pageIds.length shouldBe 10
  }

  "sending ingest results" should "provide page metadata" in {
    val job = createJob()
    val (pageId :: Nil) = server.ingest(job.id, 1)

    val metadata = Map("width" -> "33823", "height" -> "70283", "type" -> "tiff")

    server.pushMetadata(pageId, "ingest", metadata)

    server.getMetadata(pageId).right.value shouldBe metadata
  }
}
