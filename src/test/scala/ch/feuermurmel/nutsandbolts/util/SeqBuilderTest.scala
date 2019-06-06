package ch.feuermurmel.nutsandbolts.util

import org.scalatest.FreeSpec

class SeqBuilderTest extends FreeSpec {
  "buildin an empty sequence works" in {
    withBuilder(Seq()) { _ => }
  }

  "building a short sequence works" in {
    withBuilder(Seq("a", "b")) { builder =>
      builder += "a"
      builder += "b"
    }
  }

  "building a long sequence works" in {
    withBuilder(Seq.fill(100)("a")) { builder =>
      (0 until 100).foreach({ _ =>
        builder += "a"
      })
    }
  }

  "adding many elements at once works" in {
    withBuilder(Seq.fill(100)("a")) { builder =>
      builder += "a"
      builder += "a"
      builder ++= Seq.fill(98)("a")
    }
  }

  "replacing an element works" in {
    withBuilder(Seq("a", "b.")) { builder =>
      builder += "a"
      builder += "b"
      builder.last += "."
    }
  }

  "accessing the last element on an empty builder throws an exception" in {
    assertThrows[Exception](new SeqBuilder[Nothing].last)
  }

  def withBuilder(result: Seq[String])(block: SeqBuilder[String] => Unit) = {
    val builder = new SeqBuilder[String]

    block(builder)

    assertResult(result)(builder.build)
  }
}
