import java.lang.Math.sqrt
import java.nio.file.{Path, Paths}

import util.MathUtil
import util.MathUtil.tau

object Main extends App {
  def cone(z1: Double, z2: Double, r1: Double, r2: Double) =
    Surface((z, _) => (z - z1) / (z2 - z1) * (r2 - r1) + r1)

  def switchSurface(surface1: Surface, surface2: Surface, height: Double) =
    Surface((z, a) => (if (z < height) surface1 else surface2)(z, a))

  def piecewiseSurface(pieces: (Surface, Double)*) =
    pieces.init.foldRight(pieces.last._1)({ case ((surface, height), result) => switchSurface(surface, result, height) })

  def stack(pieces: (Surface, Double)*) = {
    var piecewisePieces = Seq[(Surface, Double)]()
    var pos = 0d

    pieces.foreach({ case (p, h) =>
      piecewisePieces :+= (p.shift(pos), pos + h)
      pos += h
    })

    piecewiseSurface(piecewisePieces: _*)
  }

  def repeatedSurface(surface: Surface, zMax: Double) =
    Surface({ (z, a) => surface(MathUtil.mod(z, zMax), a) })

  def skewedSurface(surface: Surface, zShift: Double) =
    Surface({ (z, a) => surface(z - a / tau * zShift, a) })

  def intersection(surfaces: Surface*) =
    Surface({ (z, a) => surfaces.map(_(z, a)).min })

  def union(surfaces: Surface*) =
    Surface({ (z, a) => surfaces.map(_(z, a)).max })

  def regularPolygon(sides: Int, innerRadius: Double) =
    intersection((0 until sides).map(i => flatSurface(innerRadius).rotate(tau * i / sides)): _*)

  def flatSurface(distance: Double) =
    Surface({ (_, a) =>
      val c = Math.cos(a)

      if (c > 0)
        distance / c
      else
        Double.PositiveInfinity
    })

  def isoThread(majorDiameter: Double, pitch: Double, includedAngle: Double = tau / 6) = {
    val h = pitch * sqrt(3) / 2

    val majorRadius = majorDiameter / 2
    val minorRadius = majorRadius - h * 5 / 8

    val height1 = pitch / 4
    val height2 = pitch * 5 / 16
    val height3 = pitch / 8
    val height4 = pitch * 5 / 16

    val piece1 = cone(0, height1, minorRadius, minorRadius)
    val piece2 = cone(0, height2, minorRadius, majorRadius)
    val piece3 = cone(0, height3, majorRadius, majorRadius)
    val piece4 = cone(0, height4, majorRadius, minorRadius)

    val surface = stack((piece1, height1), (piece2, height2), (piece3, height3), (piece4, height4))

    skewedSurface(repeatedSurface(surface, pitch), pitch)
  }

  case class Screw(size: Double, pitch: Double, headWidth: Double) {
    val radius = size / 2
    val headHeight = headWidth / 2
    val tolerance = 0.2

    val nutChamfer = headWidth * 0.03
    val nutInnerChamfer = size * 0.03

    // Chamfer used to taper the end of the screw into a circle on the pointy end of the threaded part and to insert a cone into which the thread fades near the screw's head.
    val threadChamfer = pitch

    private def headSurface = {
      val headInnerRadius = headWidth / 2

      intersection(
        regularPolygon(6, headInnerRadius),
        cone(0, nutChamfer, headInnerRadius - nutChamfer, headInnerRadius),
        cone(headHeight, headHeight - nutChamfer, headInnerRadius - nutChamfer, headInnerRadius))
    }

    private def threadSurface(offset: Double) =
      isoThread(size, pitch).radialShift(offset)

    def screw(length: Double) = {
      val thread = intersection(
        threadSurface(-tolerance / 2),
        cone(length, length - threadChamfer, radius - threadChamfer, radius))

      val surface = stack(
        (headSurface, headHeight),
        (union(thread, cone(0, threadChamfer, radius, radius - threadChamfer)), length))

      Part(surface, None, 0, length + headHeight)
    }

    def nut = {
      val thread = union(
        threadSurface(tolerance / 2),
        cone(0, nutInnerChamfer, radius + nutInnerChamfer, radius),
        cone(headHeight, headHeight - nutInnerChamfer, radius + nutInnerChamfer, radius))

      Part(headSurface, Some(thread), 0, headHeight)
    }
  }

  def writePart(part: Part, name: String): Unit = {
    def write(zResolution: Double, aPoints: Int, dirPath: Path): Unit = {
      val path = dirPath.resolve(s"$name.stl")

      println(s"Writing $path ...")

      part.toPolyhedron(zResolution, tau / aPoints).writeToSTLFile(path)
    }

    write(0.5, 6 * 6, Paths.get("output/preview"))
    write(0.1, 6 * 30, Paths.get("output"))
  }

  def writeNutAndScrew(name: String, screw: Screw, screwLength: Double): Unit = {
    writePart(screw.nut, s"$name-nut")
    writePart(screw.screw(screwLength), s"$name-screw")
  }

  def main(): Unit = {
    writeNutAndScrew("M2", Screw(2, 0.4, 4), 8)
    writeNutAndScrew("M3", Screw(3, 0.5, 5.5), 9)
    writeNutAndScrew("M4", Screw(4, 0.7, 7), 10)
    writeNutAndScrew("M5", Screw(5, 0.8, 8), 11)
    writeNutAndScrew("M6", Screw(6, 1, 10), 12)
    writeNutAndScrew("M8", Screw(8, 1.25, 13), 16)

    writeNutAndScrew("M10", Screw(10, 1.5, 16), 20)
    writeNutAndScrew("M24", Screw(24, 3, 36), 50)
  }

  main()
}
