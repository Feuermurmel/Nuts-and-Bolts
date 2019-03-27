import better.files.File

case class Point(x: Double, y: Double, z: Double)

case class Face(p1: Point, p2: Point, p3: Point)

case class Polyhedron(faces: Seq[Face]) {
  import Polyhedron._

  def asOpenSCADExpression = {
    val points = faces.flatMap(facePoints).distinct.sortBy(pointComponents)
    val indexByPoint = points.zipWithIndex.toMap

    def arrayStr[A](parts: Seq[A])(toString: A => String) =
      s"[${parts.map(toString).mkString(", ")}]"

    val pointsStr = arrayStr(points)(p => arrayStr(pointComponents(p))(_.toString))
    val facesStr = arrayStr(faces)(f => arrayStr(facePoints(f))(indexByPoint(_).toString))

    s"polyhedron(points = $pointsStr, faces = $facesStr)"
  }

  def writeToOpenSCADFile(path: File): Unit = {
    val fileContent = s"$asOpenSCADExpression;\n"

    usingTemporaryFile(path.path) { tempPath =>
      Files.write(tempPath, fileContent.getBytes(StandardCharsets.UTF_8))
    }
  }
}

object Polyhedron {
  def facePoints(face: Face) = Seq(face.p1, face.p2, face.p3)

  def pointComponents(point: Point) = Seq(point.x, point.y, point.z)
}
