import java.lang.Math.sqrt
import java.nio.file.{Path, Paths}

import Surface.{cone, coneSegment, plane, select}
import util.MathUtil
import util.MathUtil.tau

object Main extends App {
  def piecewiseSurface(pieces: (Surface, Double)*) =
    pieces.init.foldRight(pieces.last._1)({ case ((surface, height), result) =>
      select((z, _) => if (z < height) surface else result)
    })

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

  def regularPolygon(sides: Int, innerRadius: Double) =
    (0 until sides).map(i => plane(innerRadius).rotate(tau * i / sides)).reduce(_ & _)

  def isoThread(majorDiameter: Double, pitch: Double, includedAngle: Double = tau / 6) = {
    val h = pitch * sqrt(3) / 2

    val majorRadius = majorDiameter / 2
    val minorRadius = majorRadius - h * 5 / 8

    val height1 = pitch / 4
    val height2 = pitch * 5 / 16
    val height3 = pitch / 8
    val height4 = pitch * 5 / 16

    val piece1 = coneSegment(0, height1, minorRadius, minorRadius)
    val piece2 = coneSegment(0, height2, minorRadius, majorRadius)
    val piece3 = coneSegment(0, height3, majorRadius, majorRadius)
    val piece4 = coneSegment(0, height4, majorRadius, minorRadius)

    val surface = stack((piece1, height1), (piece2, height2), (piece3, height3), (piece4, height4))

    skewedSurface(repeatedSurface(surface, pitch), pitch)
  }

  case class Screw(size: Double, pitch: Double, headWidth: Double) {
    val radius = size / 2
    val headHeight = headWidth / 2
    val tolerance = 0.2

    val nutChamfer = headWidth * 0.03
    val nutInnerChamfer = size * 0.03

    // Chamfer used to taper the end of the screw into a circle on tip of the screw and near the head.
    val threadChamfer = pitch

    private def headSurface = {
      val headInnerRadius = headWidth / 2

      (
        regularPolygon(6, headInnerRadius)
          & cone(0, headInnerRadius, 1).grow(-nutChamfer)
          & cone(headHeight, headInnerRadius, -1).grow(-nutChamfer))
    }

    private def threadSurface(offset: Double) =
      isoThread(size, pitch).grow(offset)

    def screw(length: Double) = {
      val thread = (
        threadSurface(-tolerance / 2)
          & cone(length, radius, -1).grow(-threadChamfer))

      val surface = stack(
        (headSurface, headHeight),
        (thread | cone(0, radius, -1), length))

      Part(surface, None, 0, length + headHeight)
    }

    def nut = {
      val thread = (
        threadSurface(tolerance / 2)
          | cone(0, radius, -1).grow(nutInnerChamfer)
          | cone(headHeight, radius, 1).grow(nutInnerChamfer))

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
    writeNutAndScrew("M20", Screw(20, 2.5, 30), 40)
    writeNutAndScrew("M24", Screw(24, 3, 36), 50)
  }

  main()
}
