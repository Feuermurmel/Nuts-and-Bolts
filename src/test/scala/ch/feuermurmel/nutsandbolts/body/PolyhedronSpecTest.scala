package ch.feuermurmel.nutsandbolts.body

import ch.feuermurmel.nutsandbolts.polyhedron.{Face, Point, Polyhedron}
import org.scalatest.exceptions.TestFailedException

class PolyhedronSpecTest extends PolyhedronSpec {
  {
    val p0 = Point(0, 0, 0)
    val p1 = Point(1, 0, 0)
    val p2 = Point(0, 1, 0)
    val p3 = Point(0, 0, 1)

    val f1 = Face(p0, p3, p2)
    val f2 = Face(p0, p1, p3)
    val f3 = Face(p0, p2, p1)
    val f4 = Face(p1, p2, p3)

    val f1r = Face(p0, p2, p3)

    "assertValidPolyhedron() accepts valid polyhedra" in {
      assertValidPolyhedron(Polyhedron(Seq()))
      assertValidPolyhedron(Polyhedron(Seq(f1, f2, f3, f4)))
    }

    "assertValidPolyhedron() rejects invalid faces" in {
      // Faces with empty area that still line up to a topologically consistent polyhedron.
      val f2z = Face(p0, p0, p3)
      val f3z = Face(p0, p2, p0)
      val f4z = Face(p0, p2, p3)

      assertThrows[TestFailedException] {
        assertValidPolyhedron(Polyhedron(Seq(f1, f2z, f3z, f4z)))
      }
    }

    "assertValidPolyhedron() rejects invalid polyhedra" in {
      // Missing face.
      assertThrows[TestFailedException] {
        assertValidPolyhedron(Polyhedron(Seq(f1, f2, f3)))
      }

      // All faces twice.
      assertThrows[TestFailedException] {
        assertValidPolyhedron(Polyhedron(Seq(f1, f2, f3, f4, f1, f2, f3, f4)))
      }

      // Empty volume.
      assertThrows[TestFailedException] {
        assertValidPolyhedron(Polyhedron(Seq(f1, f1r)))
      }
    }
  }
}
