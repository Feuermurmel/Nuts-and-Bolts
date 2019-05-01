import java.nio.file.Paths

import util.MathUtil.tau

object Main extends App {
  case class Screw(size: Double, pitch: Double, headSize: Double)

  case class Washer(innerDiameter: Double, outerDiameter: Double, thickness: Double) {
    val tolerance = 0.2

    def surface = Part.withHole(
      Surface.cylinder(outerDiameter / 2).slice(thickness),
      Surface.cylinder(innerDiameter / 2 + tolerance / 2).slice(thickness).invert)
  }

  val outputPath = Paths.get("output")

  def writePart(part: Part, name: String): Unit = {
    val path = outputPath.resolve(s"$name.stl")

    println(s"Writing $path ...")

    val zResolution = 0.1
    val aPoints = 6 * 30

    part.toPolyhedron(zResolution, tau / aPoints).writeToSTLFile(path)
  }

  def writeNutAndScrew(name: String, screw: Screw, screwLength: Double): Unit = {
    val thread = NutsAndBolts.isoThread(screw.size, screw.pitch)
    val head = NutsAndBolts.isoBoltHead(screw.headSize)

    writePart(NutsAndBolts.Rod.nut(thread, head), s"$name-nut")
    writePart(NutsAndBolts.Rod.simpleBolt(thread, head).part(screwLength), s"$name-screw")
  }

  def writeWasher(name: String, washer: Washer): Unit =
    writePart(washer.surface, s"$name-washer")

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

    writeWasher("M8", Washer(8.4, 16, 1.6))
    writeWasher("M10", Washer(10.5, 20, 2))
  }

  main()
}
