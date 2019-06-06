package ch.feuermurmel.nutsandbolts.body

import ch.feuermurmel.nutsandbolts.polyhedron.{Point, Polyhedron}
import ch.feuermurmel.nutsandbolts.util.Counter
import org.scalatest.FreeSpec

trait PolyhedronSpec extends FreeSpec {
  def assertValidPolyhedron(polyhedron: Polyhedron) = {
    val edgeCounter = new Counter[(Point, Point)]
    val faceCounter = new Counter[(Point, Point, Point)]

    def addEdge(point1: Point, point2: Point) = {
      assert(point1 != point2, s"Edge ${(point1, point2)} has zero length.")

      val (points, parity) = Body.normalizeEdge(point1, point2)

      // Add half-edges in one direction and remove in the reverse direction with the expectation that all half-edges are cancelled out in the end.
      if (parity)
        edgeCounter -= points
      else
        edgeCounter += points
    }

    polyhedron.faces.foreach({ face =>
      import face.{p1, p2, p3}

      addEdge(p1, p2)
      addEdge(p2, p3)
      addEdge(p3, p1)

      val (normalizedPoints, _) = Body.normalizeFace(p1, p2, p3)

      // Add faces with the information of their orientation removed with the expectation that no faces should be exactly incident with the cases appearing in tests.
      faceCounter += normalizedPoints
    })

    val unbalancedEdges = edgeCounter.entries

    assert(unbalancedEdges.isEmpty, s"Polyhedron contains unconnected edges: $unbalancedEdges")

    val doubleFaces = faceCounter.entries.filter(_._2 != 1)

    assert(doubleFaces.isEmpty, s"Polyhedron has faces that occur more than once: $doubleFaces")
  }

  def assertPolyhedronProperties(polyhedron: Polyhedron, faceCount: Int): Unit = {
    assertValidPolyhedron(polyhedron)
    assertResult(faceCount)(polyhedron.faces.size)
  }
}
