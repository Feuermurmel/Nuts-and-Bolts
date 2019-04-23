import java.nio.file.Path
import java.nio.{ByteBuffer, ByteOrder}

import util.PathUtil

case class Point(x: Double, y: Double, z: Double) {
  def +(other: Point) = Point(x + other.x, y + other.y, z + other.z)
  def *(other: Double) = Point(x * other, y * other, z * other)
  def /(other: Double) = this * (1 / other)
  def -(other: Point) = this + -other
  def unary_- = this * -1

  def dot(other: Point) = x * other.x + y * other.y + z * other.z
  def normSq = this dot this
  def norm = Math.sqrt(normSq)
  def normalized = this / norm

  def cross(other: Point) =
    Point(
      y * other.z - z * other.y,
      z * other.x - x * other.z,
      x * other.y - y * other.x)
}

case class Face(p1: Point, p2: Point, p3: Point)

case class Polyhedron(faces: Seq[Face]) {
  import Polyhedron._

  def writeToSTLFile(path: Path): Unit =
    PathUtil.writeUsingChannel(path) { channel =>
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
        // TODO: Maybe we should handle polygons with zero area.
        val normal = ((face.p2 - face.p1) cross (face.p3 - face.p1)).normalized

        pointComponents(normal).foreach({ component => putFloat(component.toFloat) })

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
