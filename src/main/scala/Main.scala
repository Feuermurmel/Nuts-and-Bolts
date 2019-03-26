import java.lang.Math.sqrt

import better.files._
import util.MathUtil
import util.MathUtil.tau

import scala.sys.process._

object Main extends App {
  def cone(z1: Double, z2: Double, r1: Double, r2: Double) =
    Surface((z, _) => (z - z1) / (z2 - z1) * (r2 - r1) + r1)

  def piecewiseSurface(pieces: (Surface, Double)*) =
    Surface({ (z, a) =>
      val (surface, _) = pieces.dropWhile({ case (_, stop) => stop < z }).headOption.getOrElse(pieces.head)

      surface(z, a)
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

    val threadChamfer = size * 0.15

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
      val studHeight: Double = 0.5

      val thread = intersection(
        threadSurface(-tolerance / 2),
        cone(0, threadChamfer, radius - threadChamfer, radius),
        cone(length, length - 0.5, radius - 0.5, radius))

      val stud = cone(0, 1, radius - 0.5, radius - 0.5)

      val surface = stack(
        (thread, length),
        (stud, studHeight),
        (headSurface, headHeight))

      Part1(surface, 0, length + studHeight + headHeight)
    }

    def nut = {
      val thread = union(
        threadSurface(tolerance / 2),
        cone(0, nutInnerChamfer, radius + nutInnerChamfer, radius),
        cone(headHeight, headHeight - nutInnerChamfer, radius + nutInnerChamfer, radius))

      Part2(headSurface, thread, 0, headHeight)
    }
  }

  def writePart(part: Part, name: String): Unit = {
    def write(zResolution: Double, aPoints: Int, dirPath: File): Unit = {
      val path = dirPath / s"$name.scad"

      println(s"Writing $path ...")

      part.toPolyhedron(zResolution, tau / aPoints).writeToOpenSCADFile(path)
    }

    write(0.5, 6 * 6, file"output/preview")
    write(0.1, 6 * 30, file"output")
  }

  def main(): Unit = {
    val m10 = Screw(10, 1.5, 16)
    writePart(m10.screw(20), "M10-screw")
    writePart(m10.nut, "M10-nut")

    val m24 = Screw(24, 3, 36)
    writePart(m24.screw(50), "M24-screw")
    writePart(m24.nut, "M24-nut")

    Seq("./openscad-to-stl.sh").!
  }

  main()
}
