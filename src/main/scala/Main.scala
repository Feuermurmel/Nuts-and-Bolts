import java.nio.file.Paths

import ch.feuermurmel.nutsandbolts.body.{Body, CylindricalBody}
import ch.feuermurmel.nutsandbolts.part.{nut, simpleBolt}
import ch.feuermurmel.nutsandbolts.util.MathUtil.tau

object Main extends App {
  case class Screw(size: Double, pitch: Double, headSize: Double) {
    def thread = ISO.isoThread(size, pitch)
    def head = ISO.isoBoltHead(headSize)
  }

  case class NutAndScrew(screw: Screw, screwLength: Double) {
    def nutBody = nut(screw.thread, screw.head)
    def simpleBoltBody = simpleBolt(screw.thread, screw.head, screwLength)
  }

  case class Washer(innerDiameter: Double, outerDiameter: Double, thickness: Double) {
    val tolerance = 0.2

    def body = {
      val outerCylinder = CylindricalBody.verticalCylinder(0, 0, outerDiameter / 2)
      val innerCylinder = CylindricalBody.verticalCylinder(0, 0, innerDiameter / 2 + tolerance / 2)

      CylindricalBody(outerCylinder / innerCylinder, 0, thickness)
    }
  }

  def addHorizontalHoleAtTop(body: CylindricalBody, distanceFromTop: Double, diameter: Double) = {
    val z = body.end - distanceFromTop
    val hole = CylindricalBody.horizontalCylinder(z, 0, 0, diameter / 2)

    body.copy(body.surface / hole)
  }

  val outputPath = Paths.get("output")

  val resolution = 0.1

  def writeBody(part: Body, fileBaseName: String): Unit = {
    val path = outputPath.resolve(s"$fileBaseName.stl")

    println(s"Writing $path ...")

    val polyhedron = part.toPolyhedron(resolution)

    polyhedron.writeToSTLFile(path)

    println(s"Wrote ${polyhedron.faces.size} faces.")
  }

  val parts = {
    // TODO: Clean up this mess.
    val screwSizes =
      Seq(
        (Screw(6, 1, 10), Washer(6.4, 12, 1.6)),
        (Screw(8, 1.25, 13), Washer(8.4, 16, 1.6)),
        (Screw(10, 1.5, 16 /*17*/), Washer(10.5, 20, 2)),
        (Screw(12, 1.75, 18 /*19*/), Washer(13, 24, 2.5)),
        (Screw(16, 2, 21 /*22*/), Washer(17, 30, 3)),
        (Screw(20, 2.5, 30), Washer(21, 37, 3)),
        (Screw(24, 3, 36), Washer(25, 44, 4)),
        (Screw(30, 3.5, 46), Washer(31, 56, 4)),
        (Screw(36, 4, 55), Washer(37, 66, 5)),
        (Screw(42, 4.5, 65), Washer(43, 78, 7)),
        (Screw(48, 5, 75), Washer(50, 92, 8)))

    def findSize(size: Double) =
      screwSizes.find(_._1.size == size).getOrElse(
        throw new Exception(s"Unsupported screw size: $size"))

    Seq(
      Part("bolt", "M l hole") { arguments =>
        val screw = findSize(arguments.get("M"))._1
        val length = arguments.get("l", 10)
        var body = NutAndScrew(screw, length).simpleBoltBody

        if (arguments.getBoolean("hole"))
          body = addHorizontalHoleAtTop(body, 4.5, 4)

        body
      },
      Part("nut", "M flat round") { arguments =>
        val screw = findSize(arguments.get("M"))._1
        val flat = arguments.getBoolean("flat")
        val round = arguments.getBoolean("round")

        if (flat) {
          if (round)
            throw new Exception("Cannot combine options flat and round.")

          val thread = ISO.isoThread(screw.size, screw.pitch)
          val head = Stuff.knurledRingNut(screw.headSize * 3 / 2)

          nut(thread, head)
        } else if (round) {
          val thread = ISO.isoThread(screw.size, screw.pitch)
          val head = Stuff.knurledRoundedRingNut(screw.headSize * 3 / 2, screw.headSize * 4 / 8)

          nut(thread, head)
        } else {
          NutAndScrew(screw, 0).nutBody
        }
      },
      Part("washer", "M") { arguments =>
        findSize(arguments.get("M"))._2.body
      })
  }

  def main(): Unit = {
    val (body, fileBaseName) = Part.run(parts, args)

    writeBody(body, fileBaseName)
  }

  main()
}
