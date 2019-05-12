package ch.feuermurmel.nutsandbolts.surface

import java.lang.Math.{cos, floorMod, max, round, sin, sqrt}

import ch.feuermurmel.nutsandbolts.polyhedron
import ch.feuermurmel.nutsandbolts.polyhedron.{Face, Point, Polyhedron}
import ch.feuermurmel.nutsandbolts.util.MathUtil.{phi, tau}

case class Body(slices: Seq[SurfaceSlice], hasHole: Boolean) {
  import Body._

  def toPolyhedron(resolution: Double) = {
    val partRadius = guessPartRadius(slices, resolution)

    // Use a resolution for cResolution * partRadius that is smaller by a factor of sqrt(3) / 2 than resolution so that the triangles of the generated mesh are equilateral at a radius of partRadius.
    val cResolution = tau / Math.ceil(partRadius * tau / resolution * sqrt(3) / 2)

    case class Row(surface: Surface, z: Double)

    def getRows(surface: Surface, start: Double, end: Double) =
      closedRange(start, end, resolution).map(Row(surface, _))

    def getPoints(row: Row, index: Int) =
      // Each row is shifted half a step around the C-axis to (ideally) produce equilateral triangles instead of pairs of right-angled triangles.
      halfOpenRange(0, tau, cResolution).map(c => point(row.surface, row.z, c + cResolution * index / 2))

    var currentZ = 0.0
    var rows = Seq[Row]()

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

    val points = rows.zipWithIndex.map({ case (x, i) => getPoints(x, i) })

    Polyhedron(
      if (hasHole)
        // To close the loop of rows, the first row has to be copied to the end and rotated to counter the effect of shifting each row half a step around the C-axis.
        faces(points :+ rotate(points.head, points.size / 2))
      else
        faces(points)
          ++ endFaces(points.head.reverse, 0)
          ++ endFaces(points.last, currentZ))
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

  private def rotate[A](seq: Seq[A], n: Int) = {
    val numElements = floorMod(n, seq.size)

    seq.drop(numElements) ++ seq.take(numElements)
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
        Seq(Face(p1, p3, p2), polyhedron.Face(p2, p3, p4))
      }).flatten
    }).flatten

  private def endFaces(row: Seq[Point], z: Double) =
    slidingPairs(cyclic(row))(polyhedron.Face(_, _, Point(0, 0, z)))

  private def quantile(values: Seq[Double], q: Double) =
    values.sorted.apply(((values.size - 1) * q).toInt)

  private def guessPartRadius(slices: Seq[SurfaceSlice], resolution: Double) = {
    val goldenAngle = tau / (1 + phi)

    // Probed layers, represented as tuples of slice and Z-height.
    val layers = slices.flatMap(s => closedRange(0, s.height, resolution).map((s, _)))

    val values = layers
      .zipWithIndex
      .map({ case ((slice, z), i) => slice.surface(z, i * goldenAngle) })

    quantile(values, 0.95)
  }
}
