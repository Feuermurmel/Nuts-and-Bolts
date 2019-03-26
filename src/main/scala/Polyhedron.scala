import better.files.File

case class Polyhedron(faces: Seq[Seq[(Double, Double, Double)]]) {
  def asOpenSCADExpression = {
    val points = faces.flatten.distinct.sorted
    val indexByPoint = points.zipWithIndex.toMap

    def arrayStr[A](parts: Seq[A])(toString: A => String) =
      s"[${parts.map(toString).mkString(", ")}]"

    val pointsStr = arrayStr(points)({ case (x, y, z) => arrayStr(Seq(x, y, z))(_.toString) })
    val facesStr = arrayStr(faces)(face => arrayStr(face)(indexByPoint(_).toString))

    s"polyhedron(points = $pointsStr, faces = $facesStr)"
  }

  def writeToOpenSCADFile(path: File) = {
    val fileContent = s"$asOpenSCADExpression;\n"

    path.parent.createDirectories()
    path.write(fileContent)
  }
}
