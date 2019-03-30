import java.nio.file.Path
import java.nio.{ByteBuffer, ByteOrder}

import util.PathUtil

case class Point(x: Double, y: Double, z: Double)

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
