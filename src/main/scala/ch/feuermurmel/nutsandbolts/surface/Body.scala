package ch.feuermurmel.nutsandbolts.surface

import java.lang.Math.{cos, max, round, sin}

import ch.feuermurmel.nutsandbolts.polyhedron
import ch.feuermurmel.nutsandbolts.polyhedron.{Face, Point, Polyhedron}
import ch.feuermurmel.nutsandbolts.util.MathUtil.tau

case class Body(slices: Seq[SurfaceSlice], hasHole: Boolean) {
  import Body._

  def toPolyhedron(zResolution: Double, aResolution: Double) = {
    def getRow(surface: Surface, z: Double) =
      halfOpenRange(0, tau, aResolution).map(point(surface, z, _))

    def getRows(surface: Surface, start: Double, end: Double) =
      closedRange(start, end, zResolution).map(getRow(surface, _))

    var currentZ = 0.0
    var rows = Seq[Seq[Point]]()

    slices.foreach({ slice =>
      slice.orientation match {
        case SurfaceSlice.Orientation.Outward =>
          val nextZ = currentZ + slice.height

          rows ++= getRows(slice.surface.shift(currentZ), currentZ, nextZ)
          currentZ = nextZ
        case SurfaceSlice.Orientation.Inward =>
          val nextZ = currentZ - slice.height

          rows ++= getRows(slice.surface.shift(nextZ), nextZ, currentZ).reverse
          currentZ = nextZ
      }
    })

    Polyhedron(
      if (hasHole)
        faces(cyclic(rows))
      else
        faces(rows)
          ++ endFaces(rows.head.reverse, 0)
          ++ endFaces(rows.last, currentZ))
  }
}

object Body {
  def withoutHole(slices: SurfaceSlice*) = Body(slices, hasHole = false)

  def withHole(slices: SurfaceSlice*) = Body(slices, hasHole = true)

  private def range(start: Double, end: Double, resolution: Double, rangeFn: (Int, Int) => Range) = {
    val size = end - start
    val steps = max(2, round(size / resolution).toInt)

    rangeFn(0, steps).map(start + _ * size / steps)
  }

  private def cyclic[A](seq: Seq[A]) = seq :+ seq.head

  private def halfOpenRange(start: Double, end: Double, resolution: Double) =
    range(start, end, resolution, _ until _)

  private def closedRange(start: Double, end: Double, resolution: Double) =
    range(start, end, resolution, _ to _)

  private def slidingPairs[A, B](seq: Seq[A])(fn: (A, A) => B) =
    seq.sliding(2).map({ case Seq(a, b) => fn(a, b) }).toSeq

  private def point(surface: Surface, z: Double, a: Double) = {
    val r = surface(z, a)

    Point(cos(a) * r, sin(a) * r, z)
  }

  private def faces(rows: Seq[Seq[Point]]) =
    slidingPairs(rows)({ (row1, row2) =>
      slidingPairs(cyclic(row1.zip(row2)))({ case ((p1, p2), (p3, p4)) =>
        Seq(Face(p1, p3, p4), polyhedron.Face(p1, p4, p2))
      }).flatten
    }).flatten

  private def endFaces(row: Seq[Point], z: Double) =
    slidingPairs(cyclic(row))(polyhedron.Face(_, _, Point(0, 0, z)))
}
