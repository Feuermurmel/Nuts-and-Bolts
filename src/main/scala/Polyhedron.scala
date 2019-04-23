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

case class Face(p1: Point, p2: Point, p3: Point) {
  // TODO: Maybe we should handle polygons with zero area.
  def normal = ((p2 - p1) cross (p3 - p1)).normalized
}

case class Polyhedron(faces: Seq[Face]) {
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

      def putPoint(point: Point) = {
        putFloat(point.x.toFloat)
        putFloat(point.y.toFloat)
        putFloat(point.z.toFloat)
      }

      // Skip unused header part.
      skip(80)

      // Face count.
      putInt(faces.size)

      flush()

      faces.foreach({ face =>
        putPoint(face.normal)
        putPoint(face.p1)
        putPoint(face.p2)
        putPoint(face.p3)

        // Attribute size.
        putShort(0)

        flush()
      })
    }
}
