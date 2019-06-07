import java.nio.file.Paths

import ch.feuermurmel.nutsandbolts.body.{Body, CylindricalBody, Interval, Ray, RectangularBody, Surface}
import ch.feuermurmel.nutsandbolts.part.nut
import ch.feuermurmel.nutsandbolts.util.MathUtil.tau

object Main extends App {
  case class Screw(size: Double, pitch: Double, headSize: Double)

//  case class Washer(innerDiameter: Double, outerDiameter: Double, thickness: Double) {
//    val tolerance = 0.2
//
//    def surface = Body.withHole(
//      Surface.cylinder(outerDiameter / 2).slice(thickness),
//      Surface.cylinder(innerDiameter / 2 + tolerance / 2).slice(thickness).invert)
//  }
//
  val outputPath = Paths.get("output")

  def writePart(part: Body, name: String): Unit = {
    val path = outputPath.resolve(s"$name.stl")

    println(s"Writing $path ...")

    val polyhedron = part.toPolyhedron(0.1)

    polyhedron.writeToSTLFile(path)

    println(s"Wrote ${polyhedron.faces.size} faces.")
  }

  def writeNutAndScrew(name: String, screw: Screw, screwLength: Double): Unit = {
    val thread = ISO.isoThread(screw.size, screw.pitch)
    val head = ISO.isoBoltHead(screw.headSize)

    writePart(nut(thread, head), s"$name-nut")
//    writePart(simpleBolt(thread, head, screwLength), s"$name-screw")
  }

//  def writeKnurledRingNut(name: String, screw: Screw): Unit = {
//    val thread = ISO.isoThread(screw.size, screw.pitch)
//    val head = Stuff.knurledRoundedRingNut(screw.headSize * 3 / 2, screw.headSize * 4 / 8)
//
//    writePart(nut(thread, head), s"$name-ring-nut")
//  }

//  def writeWasher(name: String, washer: Washer): Unit =
//    writePart(washer.surface, s"$name-washer")

  def main(): Unit = {
//    writeNutAndScrew("M2", Screw(2, 0.4, 4), 8)
//    writeNutAndScrew("M3", Screw(3, 0.5, 5.5), 9)
//    writeNutAndScrew("M4", Screw(4, 0.7, 7), 10)
//    writeNutAndScrew("M5", Screw(5, 0.8, 8), 11)
//    writeNutAndScrew("M6", Screw(6, 1, 10), 12)
//    writeNutAndScrew("M8", Screw(8, 1.25, 13), 16)

    writeNutAndScrew("M10", Screw(10, 1.5, 16), 20)
//    writeKnurledRingNut("M10", Screw(10, 1.5, 16))
//
//    writeNutAndScrew("M20", Screw(20, 2.5, 30), 40)
//    writeNutAndScrew("M24", Screw(24, 3, 36), 50)
//    writeKnurledRingNut("M24", Screw(24, 3, 36))
//
//    writeWasher("M8", Washer(8.4, 16, 1.6))
//    writeWasher("M10", Washer(10.5, 20, 2))


//    val block = Surface(_ => Ray(Seq(Interval(-10, 10))))
    val cylinder1 = RectangularBody.horizontalCylinder(0, 0, 0, 1, 1, 5)
    val cylinder2 = RectangularBody.horizontalCylinder(0, 0, 0, 1, -1, 3)

    val part = RectangularBody(cylinder1 / cylinder2, -10, 10, -10, 10)

    writePart(part, "test")
  }

  main()
}
