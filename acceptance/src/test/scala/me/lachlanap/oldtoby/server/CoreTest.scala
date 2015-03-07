package me.lachlanap.oldtoby.server

import me.lachlanap.oldtoby.server.helpers.Up

class CoreTest extends UnitSpec {

  "server status" should "be online" in {
    val status = server.status

    status shouldBe a[Up]
  }
}
