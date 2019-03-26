import java.lang.Math.{cos, sin}

import util.MathUtil.tau
import util.{MathUtil, SeqUtil}

trait Part {
  def toPolyhedron(zResolution: Double, aResolution: Double): Polyhedron
}

case class Part1(outerSurface: Surface, zMin: Double, zMax: Double) extends Part {
  def toPolyhedron(zResolution: Double, aResolution: Double) = {
    def point(z: Double, a: Double) = {
      val r = outerSurface(z, a)

      Point(cos(a) * r, sin(a) * r, z)
    }

    val rows = MathUtil
      .inclusiveRangeWithResolution(zMin, zMax, zResolution)
      .map({ z =>
        MathUtil
          .rangeWithResolution(0, tau, aResolution)
          .map(a => point(z, a))
      })

    val verticalFaces = SeqUtil
      .mapSlidingPairs(rows)({ (row1, row2) =>
        SeqUtil
          .mapCyclicSlidingPairs(row1.zip(row2))({ case ((p1, p2), (p3, p4)) =>
            Seq(Face(p1, p2, p4), Face(p1, p4, p3))
          })
          .flatten
      })
      .flatten

    def endFaces(z: Double, row: Seq[Point]) =
      SeqUtil.mapCyclicSlidingPairs(row)((p1, p2) => Face(p1, p2, Point(0, 0, z)))

    Polyhedron(verticalFaces ++ endFaces(zMin, rows.head) ++ endFaces(zMax, rows.last))

  }
}

case class Part2(outerSurface: Surface, innerSurface: Surface, zMin: Double, zMax: Double) extends Part {
  def toPolyhedron(zResolution: Double, aResolution: Double) = {
    def point(surface: Surface, z: Double, a: Double) = {
      val r = surface(z, a)

      Point(cos(a) * r, sin(a) * r, z)
    }

    val outerRows = MathUtil
      .inclusiveRangeWithResolution(zMin, zMax, zResolution)
      .map({ z =>
        MathUtil
          .rangeWithResolution(0, tau, aResolution)
          .map(a => point(outerSurface, z, a))
      })

    val innerRows = MathUtil
      .inclusiveRangeWithResolution(zMin, zMax, zResolution)
      .map({ z =>
        MathUtil
          .rangeWithResolution(0, tau, aResolution)
          .map(a => point(innerSurface, z, a))
      })

    val verticalFaces = SeqUtil
      .mapCyclicSlidingPairs(outerRows ++ innerRows.reverse)({ (row1, row2) =>
        SeqUtil
          .mapCyclicSlidingPairs(row1.zip(row2))({ case ((p1, p2), (p3, p4)) =>
            Seq(Face(p1, p2, p4), Face(p1, p4, p3))
          })
          .flatten
      })
      .flatten

    Polyhedron(verticalFaces)
  }
}
