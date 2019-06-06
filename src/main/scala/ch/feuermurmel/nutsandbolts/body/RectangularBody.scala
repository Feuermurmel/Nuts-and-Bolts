package ch.feuermurmel.nutsandbolts.body

import java.lang.Math.sqrt

import ch.feuermurmel.nutsandbolts.polyhedron.Point
import ch.feuermurmel.nutsandbolts.util.MathUtil.closedRange

case class RectangularBody(surface: Surface, xStart: Double, xEnd: Double, yStart: Double, yEnd: Double) extends Body {
  override def toPolyhedron(resolution: Double) = {
    val xRange = closedRange(xStart, xEnd, resolution)
    val yRange = closedRange(yStart, yEnd, resolution)

    def combineIndex(i: Int, j: Int) =
      Parameter(xRange(i), yRange(j))

    def pointOnRay(parameter: Parameter, value: Double) =
      Point(parameter.z, parameter.c, value)

    Body.constructPolyhedron[Parameter](xRange.size, yRange.size, combineIndex, surface(_), pointOnRay)
  }
}

object RectangularBody {
//  def verticalCylinder(x: Double, y: Double)

  def horizontalCylinder(x0: Double, y0: Double, z0: Double, dx: Double, dy: Double, r: Double) = {
    val norm = sqrt(dx * dx + dy * dy)
    val dxn = dx / norm
    val dyn = dy / norm

    Surface({ parameter =>
      val px = parameter.z
      val py = parameter.c

      val d = dyn * (px - x0) - dxn * (py - y0)
      val zOffset = sqrt(r * r - d * d)

      if (zOffset.isNaN) {
        Ray.empty
      } else {
        val zStart = z0 - zOffset
        val zEnd = z0 + zOffset

        if (zStart == zEnd)
          Ray.empty
        else
          Interval(zStart, zEnd)
      }
    })
  }
}
