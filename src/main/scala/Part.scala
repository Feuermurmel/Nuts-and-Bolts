import java.lang.Math.{cos, max, round, sin}

import util.MathUtil.tau

case class Part(outerSurface: Surface, innerSurface: Option[Surface], zMin: Double, zMax: Double) {
  import Part._

  def toPolyhedron(zResolution: Double, aResolution: Double) = {
    def row(surface: Surface, z: Double) =
      halfOpenRange(0, tau, aResolution).map(point(surface, z, _))

    def rows(surface: Surface) =
      closedRange(zMin, zMax, zResolution).map(row(surface, _))

    val outerRows = rows(outerSurface)

    val allFaces = innerSurface.map(rows) match {
      case None => (
        faces(rows(outerSurface))
          ++ endFaces(outerRows.head.reverse, zMin)
          ++ endFaces(outerRows.last, zMax))
      case Some(innerRows) =>
        faces(cyclic(outerRows ++ innerRows.reverse))
    }

    Polyhedron(allFaces)
  }
}

object Part {
  def range(start: Double, end: Double, resolution: Double, rangeFn: (Int, Int) => Range) = {
    val size = end - start
    val steps = max(2, round(size / resolution).toInt)

    rangeFn(0, steps).map(start + _ * size / steps)
  }

  def cyclic[A](seq: Seq[A]) = seq :+ seq.head

  def halfOpenRange(start: Double, end: Double, resolution: Double) =
    range(start, end, resolution, _ until _)

  def closedRange(start: Double, end: Double, resolution: Double) =
    range(start, end, resolution, _ to _)

  def slidingPairs[A, B](seq: Seq[A])(fn: (A, A) => B) =
    seq.sliding(2).map({ case Seq(a, b) => fn(a, b) }).toSeq

  def point(surface: Surface, z: Double, a: Double) = {
    val r = surface(z, a)

    Point(cos(a) * r, sin(a) * r, z)
  }

  def faces(rows: Seq[Seq[Point]]) =
    slidingPairs(rows)({ (row1, row2) =>
      slidingPairs(cyclic(row1.zip(row2)))({ case ((p1, p2), (p3, p4)) =>
        Seq(Face(p1, p3, p4), Face(p1, p4, p2))
      }).flatten
    }).flatten

  def endFaces(row: Seq[Point], z: Double) =
    slidingPairs(cyclic(row))(Face(_, _, Point(0, 0, z)))
}
