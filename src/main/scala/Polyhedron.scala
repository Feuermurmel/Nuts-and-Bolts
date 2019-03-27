import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.{ByteBuffer, ByteOrder}

import better.files.File
import util.PathUtil
import util.PathUtil.usingTemporaryFile

case class Point(x: Double, y: Double, z: Double)

case class Face(p1: Point, p2: Point, p3: Point)

case class Polyhedron(faces: Seq[Face]) {
  import Polyhedron._

  def asOpenSCADExpression = {
    val points = faces.flatMap(facePoints).distinct.sortBy(x => pointComponents(x): Iterable[Double])
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

  def writeToSTLFile(path: File): Unit =
    PathUtil.writeUsingChannel(path.path) { channel =>
      val buffer = ByteBuffer.allocate(80 + 4)
      buffer.order(ByteOrder.LITTLE_ENDIAN)

      import buffer.{putFloat, putInt, putShort}

      def flush(): Unit = {
        buffer.flip()
        channel.write(buffer)
        buffer.clear()
      }

      def skip(count: Int) =
        buffer.position(buffer.position() + count)

      // Skip unused header part.
      skip(80)

      // Face count.
      putInt(faces.size)

      flush()

      faces.foreach({ face =>
        // Skip the normal (leave all zeros).
        skip(3 * 4)

        facePoints(face).foreach({ point =>
          pointComponents(point).foreach({ component =>
            putFloat(component.toFloat)
          })
        })

        // Attribute size.
        putShort(0)

        flush()
      })
    }
}

object Polyhedron {
  def facePoints(face: Face) = Seq(face.p1, face.p2, face.p3)

  def pointComponents(point: Point) = Seq(point.x, point.y, point.z)
}
