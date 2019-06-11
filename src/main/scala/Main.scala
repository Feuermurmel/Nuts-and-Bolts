import java.nio.file.Paths

import ch.feuermurmel.nutsandbolts.body.{Body, CylindricalBody}
import ch.feuermurmel.nutsandbolts.part.{nut, simpleBolt}
import ch.feuermurmel.nutsandbolts.util.MathUtil.tau

object Main extends App {
  val outputPath = Paths.get("output")

  def writePart(part: Part): Unit = {
    val path = outputPath.resolve(s"${part.fileBaseName}.stl")

    println(s"Writing $path ...")

    val polyhedron = part.body.toPolyhedron(part.resolution)

    polyhedron.writeToSTLFile(path)

    println(s"Wrote ${polyhedron.faces.size} faces.")
  }

  val partDefs = {
    case class Screw(size: Double, pitch: Double, headSize: Double)

    case class Washer(innerDiameter: Double, outerDiameter: Double, thickness: Double)

    def addHorizontalHoleAtTop(body: CylindricalBody, distanceFromTop: Double, diameter: Double) = {
      val z = body.end - distanceFromTop
      val hole = CylindricalBody.horizontalCylinder(z, 0, 0, diameter / 2)

      body.copy(body.surface / hole)
    }

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
      Part.define("bolt", "M l hole") { arguments =>
        val screw = findSize(arguments.get("M"))._1
        val length = arguments.get("l", 10)
        val thread = ISO.isoThread(screw.size, screw.pitch)
        val head = ISO.isoBoltHead(screw.headSize)
        var body = simpleBolt(thread, head, length)

        if (arguments.getBoolean("hole"))
          body = addHorizontalHoleAtTop(body, 4.5, 4)

        body
      },
      Part.define("nut", "M flat round") { arguments =>
        val screw = findSize(arguments.get("M"))._1
        val thread = ISO.isoThread(screw.size, screw.pitch)

        val flat = arguments.getBoolean("flat")
        val round = arguments.getBoolean("round")

        val head = if (flat) {
          if (round)
            throw new Exception("Cannot combine options flat and round.")

          Stuff.knurledRingNut(screw.headSize * 3 / 2)
        } else if (round) {
          Stuff.knurledRoundedRingNut(screw.headSize * 3 / 2, screw.headSize * 4 / 8)
        } else {
          ISO.isoBoltHead(screw.headSize)
        }

        nut(thread, head)
      },
      Part.define("washer", "M hex") { arguments =>
        val washer = findSize(arguments.get("M"))._2

        val outerSurface =
          if (arguments.getBoolean("hex"))
            ISO.isoBoltHead(washer.outerDiameter).surface
          else
            CylindricalBody.verticalCylinder(0, 0, washer.outerDiameter / 2)

        val innerSurface = CylindricalBody.verticalCylinder(0, 0, washer.innerDiameter / 2)

        val surface = outerSurface / innerSurface

        CylindricalBody(surface, 0, washer.thickness)
      })
  }

  def main(): Unit = writePart(Part.run(partDefs, args))

  main()
}
