package ch.feuermurmel.nutsandbolts.polyhedron

import ch.feuermurmel.nutsandbolts.util.PathUtil
import org.scalatest.FreeSpec

class PolyhedronTest extends FreeSpec {
  import Double.{NaN => nan, PositiveInfinity => inf}

  val p0 = Point(0, 0, 0)
  val p1 = Point(0, 0, 1)
  val p2 = Point(0, 0, 2)

  "writing a polyhedron with invalid coordinates fails" in {
    def tryWrite(polyhedron: Polyhedron) =
      assertThrows[IllegalArgumentException] {
        PathUtil.usingTemporaryDirectory { path =>
          polyhedron.writeToSTLFile(path.resolve("test.stl"))
        }
      }

    tryWrite(Polyhedron(Seq(Face(p0, p1, Point(0, 0, nan)))))
    tryWrite(Polyhedron(Seq(Face(p0, p1, Point(0, 0, inf)))))
  }

  "writing a polyhedron with zero area works" in {
    def tryWrite(polyhedron: Polyhedron) =
      PathUtil.usingTemporaryDirectory { path =>
        polyhedron.writeToSTLFile(path.resolve("test.stl"))
      }

    tryWrite(Polyhedron(Seq(Face(p0, p0, p0))))
    tryWrite(Polyhedron(Seq(Face(p0, p0, p1))))
    tryWrite(Polyhedron(Seq(Face(p0, p1, p2))))
  }
}
